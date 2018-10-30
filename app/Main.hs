{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Error
import           Control.Exception        (bracket)
import qualified Control.Logging          as Log
import           Control.Monad
import qualified Data.PQueue.Prio.Min     as PQueue
import           Data.Scientific
import qualified Data.Text                as Text
import           Data.Time.Clock
import           Database
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres   as Pg
import           Network.HostName         (getHostName)

import           Check.DiskSpaceUsage
import           Check.ProcessStatistics

data Job = Job
  { jobDescription  :: Text.Text
  , jobAction       :: IO ()
  , jobInterval     :: Maybe NominalDiffTime
  , jobResult       :: MVar Bool
  }

newPeriodicJob :: Text.Text -> IO () -> NominalDiffTime -> IO Job
newPeriodicJob desc action interval = do
  result <- newMVar True
  return
    Job { jobDescription  = desc
        , jobAction       = action
        , jobInterval     = Just interval
        , jobResult       = result
        }

newStreamingJob :: Text.Text -> IO () -> IO Job
newStreamingJob desc action = do
  result <- newMVar True
  return
    Job { jobDescription  = desc
        , jobAction       = action
        , jobInterval     = Nothing
        , jobResult       = result
        }

type JobQueue = PQueue.MinPQueue UTCTime Job

data Running = Running (Async ()) (MVar Bool)

data Scheduler = Scheduler
  { queue   :: JobQueue
  , running :: [Running]
  }

data Message m where
  SampleData :: m () -> Message m
  Shutdown :: Message m

type MessageQueue m = TBQueue (Message m)

sendMessage :: MessageQueue m -> Message m -> STM ()
sendMessage = writeTBQueue

recvMessage :: MessageQueue m -> STM (Message m)
recvMessage = readTBQueue

main :: IO ()
main = Log.withStdoutLogging $ do
  msgq <- atomically $ newTBQueue 1000
  hostname <- Text.pack <$> getHostName
  now <- getCurrentTime
  Log.log $ Text.unwords
    [ "Meerkat started on"
    , hostname
    , "at"
    , showTxt now
    ]

  df <- newPeriodicJob "Check disk space usage" (diskspace msgq hostname) 60
  pidstat1 <- newPeriodicJob "Check process statistics" (pidstats msgq hostname) 120
  pidstat2 <- newPeriodicJob "Check process statistics" (pidstats msgq hostname) 120
  let queue = PQueue.fromList
                [ (now, df)
                -- run two instances of pidstat, each triggered every 2 mins
                -- each one is expected to collect stats for 1 minute
                , (now, pidstat1)
                , (addUTCTime 60 now, pidstat2)
                ]
  _ <- forkIO $ dbLogger msgq
  runScheduler (Scheduler queue [])

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
          Pg.runBeamPostgresDebug (Log.debug . Text.pack) conn insertAction
          loop conn
        Shutdown -> return ()

type InsertValueSyntax cmd = Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)))

diskspace
  :: forall cmd be hdl m.
     ( IsSql92Syntax cmd
     , MonadBeam cmd be hdl m
     , HasSqlValueSyntax (InsertValueSyntax cmd) Integer
     , HasSqlValueSyntax (InsertValueSyntax cmd) Text.Text
     , HasSqlValueSyntax (InsertValueSyntax cmd) UTCTime
     )
  => MessageQueue m
  -> Text.Text
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

pidstats
  :: forall cmd be hdl m.
     ( IsSql92Syntax cmd
     , MonadBeam cmd be hdl m
     , HasSqlValueSyntax (InsertValueSyntax cmd) (Maybe Integer)
     , HasSqlValueSyntax (InsertValueSyntax cmd) Text.Text
     , HasSqlValueSyntax (InsertValueSyntax cmd) (Maybe Scientific)
     , HasSqlValueSyntax (InsertValueSyntax cmd) UTCTime
     )
  => MessageQueue m
  -> Text.Text
  -> IO ()
pidstats msgq hostname =
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (processes hostname)
  where
    insertAction :: [ProcessStats] -> m ()
    insertAction = runInsert . insert (dbProcessStats db) . insertValues

runScheduler :: Scheduler -> IO ()
runScheduler scheduler = do
  (scheduler', delay) <- schedule scheduler
  unless (nothingScheduled scheduler') $ do
    let maxDelay = if null (running scheduler') then 10 else 1
    sleep maxDelay delay
    runScheduler scheduler'
  where
    sleep maxDelay t
      | t <= 0 = return ()
      | otherwise = threadDelay $ ceiling (min t maxDelay * 1e6)
          -- Log.log $ "Sleep " <> showTxt t <> ", max " <> showTxt maxDelay

nothingScheduled :: Scheduler -> Bool
nothingScheduled Scheduler {..} =
  PQueue.null queue && null running

schedule :: Scheduler -> IO (Scheduler, NominalDiffTime)
schedule s = do
  now <- getCurrentTime
  reap s >>= sow now

-- Check to see if any running jobs have completed and
-- 1) write their results to the result MVar
-- 2) remove them from the running set
reap :: Scheduler -> IO Scheduler
reap Scheduler {..} = do
  running' <- concat <$> mapM reaper running
  return Scheduler { running = running', .. }
  where
    reaper (Running a mvar) = do
      result <- poll a
      case result of
        Nothing ->
          return [Running a mvar]
        Just (Left e) -> do
          Log.log $ "Job raised exception: " <> showTxt e
          putMVar mvar False
          return []
        Just (Right ()) -> do
          Log.log "Job completed"
          putMVar mvar True
          return []

-- Check job queue for new jobs that are ready to run.
sow :: UTCTime -> Scheduler -> IO (Scheduler, NominalDiffTime)
sow now s@Scheduler {..} =
  case PQueue.getMin queue of
    Nothing -> return (s, 0)
    Just (jobTime, job) ->
      if now < jobTime
      then
        return (s, diffUTCTime jobTime now)
      else
        start s jobTime job >>= sow now

-- Attempt to start a job if it's not already running, and add it to the
-- running set.
start :: Scheduler -> UTCTime -> Job -> IO Scheduler
start s@Scheduler {..} jobTime job = do
  Log.log $ "Starting: " <> jobDescription job <> " scheduled for " <> showTxt jobTime
  result <- tryTakeMVar (jobResult job)
  s' <- case result of
    Nothing -> do
      Log.log "Skipping job, previous instance still busy"
      return s
    Just _ -> do
      a <- async $ jobAction job
      return s { running = Running a (jobResult job) : running }
  return s' { queue = reschedule jobTime job queue }

-- Reschedule the job on the top of the jobqueue
-- 1) Remove it from the top
-- 2) Add it at back at jobTime + jobInterval if one is set
reschedule :: UTCTime -> Job -> JobQueue -> JobQueue
reschedule jobTime job =
  case jobInterval job of
    Just interval ->
      PQueue.insert (addUTCTime interval jobTime) job . PQueue.deleteMin
    Nothing ->
      PQueue.deleteMin

showTxt :: Show a => a -> Text.Text
showTxt = Text.pack . show
