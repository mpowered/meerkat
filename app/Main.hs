{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Error
import           Control.Exception        (bracket)
import qualified Data.PQueue.Prio.Min     as PQueue
import           Data.Text.Lazy           as Text
import           Data.Time.Clock
import           Database
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres   as Pg
import           Network.HostName         (getHostName)

import           Check.DiskSpaceUsage

data Job = Job
  { jobAction   :: IO ()
  , jobInterval :: NominalDiffTime
  , jobLast     :: Maybe (Async ())
  }

type JobQueue = PQueue.MinPQueue UTCTime Job

data Message m where
  SampleData :: m () -> Message m
  Shutdown :: Message m

type MessageQueue m = TBQueue (Message m)

sendMessage :: MessageQueue m -> Message m -> STM ()
sendMessage = writeTBQueue

recvMessage :: MessageQueue m -> STM (Message m)
recvMessage = readTBQueue

main :: IO ()
main = do
  msgq <- atomically $ newTBQueue 1000
  hostname  <- Text.pack <$> getHostName
  now <- getCurrentTime
  let job = Job (diskspace msgq hostname) 60 Nothing
      queue = PQueue.fromList [ (now, job) ]
  _ <- forkIO $ dbLogger msgq
  scheduler queue

dbLogger
  :: MessageQueue Pg.Pg
  -> IO ()
dbLogger msgq =
  bracket
   ( Pg.connect
      Pg.defaultConnectInfo
        { Pg.connectHost = "10.0.0.8"
        , Pg.connectUser = "tsdb"
        , Pg.connectPassword = "tsdb"
        , Pg.connectDatabase = "tsdb"
        }
    )
    Pg.close
    loop
  where
    loop conn = do
      msg <- atomically $ recvMessage msgq
      case msg of
        SampleData insertAction -> do
          Pg.runBeamPostgresDebug putStrLn conn insertAction
          loop conn
        Shutdown -> return ()

type InsertValueSyntax cmd = Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)))

diskspace
  :: forall cmd be hdl m.
     ( IsSql92Syntax cmd
     , MonadBeam cmd be hdl m
     , HasSqlValueSyntax (InsertValueSyntax cmd) Integer
     , HasSqlValueSyntax (InsertValueSyntax cmd) Text
     , HasSqlValueSyntax (InsertValueSyntax cmd) UTCTime
     )
  => MessageQueue m
  -> Text
  -> IO ()
diskspace msgq hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (freespace hostname now)
  where
    insertAction :: [DiskSpaceUsage] -> m ()
    insertAction = runInsert . insert (dbDiskSpaceUsage db) . insertValues

scheduler :: JobQueue -> IO ()
scheduler queue = do
  now <- getCurrentTime
  case PQueue.getMin queue of
    Nothing -> return ()
    Just (jobTime, job) ->
      if now < jobTime
      then do
        sleep (diffUTCTime jobTime now)
        scheduler queue
      else do
        job' <- startJob job
        scheduler (reschedule jobTime job' queue)
  where
    startJob job =
      case jobLast job of
        Nothing -> do
          a' <- async $ jobAction job
          return job { jobLast = Just a' }
        Just a -> do
          ap <- poll a
          case ap of
            Nothing -> do
              putStrLn "Skipping job, previous instance still busy"
              return job
            Just (Left e) -> do
              putStrLn $ "Previous job raised exception: " ++ show e
              return job
            Just (Right ()) -> do
              a' <- async $ jobAction job
              return job { jobLast = Just a' }
    sleep t
      | t < 0 = return ()
      | otherwise = do
        -- cap sleep to some maximum (sometimes local clock can jump)
        let micro = if t > 10 then 10e6 else ceiling (t * 1e6)
        threadDelay micro

    reschedule jobTime job =
      PQueue.insert (addUTCTime (jobInterval job) jobTime) job . PQueue.deleteMin
