{-# OPTIONS_GHC -Wno-orphans       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Error
import           Control.Exception
import qualified Control.Logging          as Log
import           Control.Monad
import           Data.Pool
import qualified Data.PQueue.Prio.Min     as PQueue
import qualified Database.Redis           as Redis
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Time.Clock
import           Data.Version
import           Data.Yaml
import           Database
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables
import qualified Database.Beam.Postgres   as Pg
import           Network.HostName         (getHostName)
import qualified Network.HTTP.Req         as Req
import qualified Options.Applicative      as OptParse
import           Options.Applicative      ((<**>))

import           Check.DiskSpaceUsage
import           Check.MemoryUsage
import           Check.Puma
import           Check.ProcessStatistics
import           Check.SidekiqQueues

import           Paths_meerkat

data Config = Config
  { cfgHostname     :: Maybe Text
  , cfgLogging      :: LogConfig
  , cfgDatabase     :: Pg.ConnectInfo
  , cfgBinaries     :: BinConfig
  , cfgSidekiq      :: Maybe SidekiqConfig
  , cfgPuma         :: Maybe PumaConfig
  }

data BinConfig = BinConfig
  { cfgDF           :: FilePath
  , cfgFree         :: FilePath
  , cfgPidstat      :: FilePath
  }

defaultBinConfig :: BinConfig
defaultBinConfig = BinConfig "df" "free" "pidstat"

data LogConfig = LogConfig
  { cfgLogFile      :: Maybe FilePath
  , cfgLogLevel     :: Log.LogLevel
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig Nothing Log.LevelInfo

data SidekiqConfig = SidekiqConfig
  { cfgSkDatabase   :: Redis.ConnectInfo
  , cfgSkQueues     :: Text
  }

data PumaConfig = PumaConfig
  { cfgPumaCtl      :: PumaCtl
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config
      <$> o .:? "hostname"
      <*> o .:? "logging"  .!= defaultLogConfig
      <*> o .:  "database"
      <*> o .:? "binaries" .!= defaultBinConfig
      <*> o .:? "sidekiq"
      <*> o .:? "puma"

instance FromJSON LogConfig where
  parseJSON = withObject "LogConfig" $ \o ->
    LogConfig
      <$> o .:? "logfile"  .!= cfgLogFile defaultLogConfig
      <*> o .:? "loglevel" .!= cfgLogLevel defaultLogConfig

instance FromJSON BinConfig where
  parseJSON = withObject "BinConfig" $ \o ->
    BinConfig
      <$> o .:? "df"       .!= cfgDF defaultBinConfig
      <*> o .:? "free"     .!= cfgFree defaultBinConfig
      <*> o .:? "pidstat"  .!= cfgPidstat defaultBinConfig

instance FromJSON SidekiqConfig where
  parseJSON = withObject "SidekiqConfig" $ \o ->
    SidekiqConfig
      <$> o .:  "database"
      <*> o .:  "queues"

instance FromJSON PumaConfig where
  parseJSON = withObject "PumaConfig" $ \o -> do
    PumaCtl url opt <- o .: "url"
    token <- o .: "token"
    return $ PumaConfig (PumaCtl url (opt <> "token" Req.=: (token::Text)))

-- Orphan
instance FromJSON Pg.ConnectInfo where
  parseJSON = withObject "ConnectInfo" $ \o ->
    Pg.ConnectInfo
      <$> o .:? "host"     .!= Pg.connectHost Pg.defaultConnectInfo
      <*> o .:? "port"     .!= Pg.connectPort Pg.defaultConnectInfo
      <*> o .:? "user"     .!= "meerkat"
      <*> o .:? "password" .!= Pg.connectPassword Pg.defaultConnectInfo
      <*> o .:? "database" .!= "meerkat"

-- Orphan
instance FromJSON Redis.ConnectInfo where
  parseJSON = withText "ConnectInfo" $
    either fail return . Redis.parseConnectInfo . Text.unpack

-- Orphan
instance FromJSON Log.LogLevel where
  parseJSON = withText "LogLevel" parseLogLevel
    where
      parseLogLevel "debug" = return Log.LevelDebug
      parseLogLevel "info"  = return Log.LevelInfo
      parseLogLevel "warn"  = return Log.LevelWarn
      parseLogLevel "error" = return Log.LevelError
      parseLogLevel txt     = fail $ "'" <> Text.unpack txt <> "' is not a valid log level"

newtype MeerkatOptions = MeerkatOptions
  { configFile :: FilePath
  }

meerkatOptions :: OptParse.Parser MeerkatOptions
meerkatOptions = MeerkatOptions <$> OptParse.strOption
  (  OptParse.long "config-file"
  <> OptParse.metavar "PATH"
  <> OptParse.short 'c'
  <> OptParse.value "meerkat.yaml"
  <> OptParse.help "Path to Meerkat configuration file"
  )

data Job = Job
  { jobDescription  :: Text
  , jobAction       :: IO ()
  , jobInterval     :: Maybe NominalDiffTime
  , jobResult       :: MVar Bool
  }

newPeriodicJob :: Text -> IO () -> NominalDiffTime -> IO Job
newPeriodicJob desc action interval = do
  result <- newMVar True
  return
    Job { jobDescription  = desc
        , jobAction       = action
        , jobInterval     = Just interval
        , jobResult       = result
        }

newStreamingJob :: Text -> IO () -> IO Job
newStreamingJob desc action = do
  result <- newMVar True
  return
    Job { jobDescription  = desc
        , jobAction       = action
        , jobInterval     = Nothing
        , jobResult       = result
        }

type JobSpec = (UTCTime, Job)

type JobQueue = PQueue.MinPQueue UTCTime Job

data Running = Running (Async ()) (MVar Bool)

data Scheduler = Scheduler
  { queue   :: JobQueue
  , running :: [Running]
  }

data Message m where
  SampleData :: m () -> Message m

type MessageQueue m = TBQueue (Message m)

sendMessage :: MessageQueue m -> Message m -> STM ()
sendMessage = writeTBQueue

recvMessage :: MessageQueue m -> STM (Message m)
recvMessage = readTBQueue

main :: IO ()
main = OptParse.execParser opts >>= initialise
  where
    opts = OptParse.info
      (meerkatOptions <**> OptParse.helper)
      (OptParse.fullDesc <> OptParse.progDesc "Meerkat" <> OptParse.header
        "Meerkat - monitors your system"
      )

    initialise MeerkatOptions{..} =
      decodeFileEither configFile >>= either parseError runApp

    parseError e = do
      putStrLn "Unable to parse configuration file"
      putStrLn $ prettyPrintParseException e

    runApp cfg@Config{..} = do
      let withLogging = maybe Log.withStdoutLogging Log.withFileLogging (cfgLogFile cfgLogging)
      withLogging $ do
        Log.setLogLevel (cfgLogLevel cfgLogging)
        app cfg

app :: Config -> IO ()
app Config{..} = do
  msgq <- atomically $ newTBQueue 1000
  shutdown <- atomically $ newTVar False
  hostname <- maybe (Text.pack <$> getHostName) return cfgHostname
  now <- getCurrentTime
  Log.log $ Text.unwords
    [ "Meerkat"
    , Text.pack (showVersion version)
    , "started on"
    , hostname
    , "at"
    , showTxt now
    ]

  df <- newPeriodicJob "Check disk space usage" (diskspace msgq (cfgDF cfgBinaries) hostname) 60
  mem <- newPeriodicJob "Check memory usage" (memory msgq (cfgFree cfgBinaries) hostname) 20
  sk <- maybe (return Nothing) (\cfg -> Just <$> newPeriodicJob "Check sidekiq queues" (sidekiq msgq cfg) 20) cfgSidekiq
  p <- maybe (return Nothing) (\cfg -> Just <$> newPeriodicJob "Poll Puma statistics" (puma msgq (cfgPumaCtl cfg) hostname) 20) cfgPuma
  pidstat1 <- newPeriodicJob "Check process statistics" (pidstats msgq (cfgPidstat cfgBinaries) hostname) 120
  pidstat2 <- newPeriodicJob "Check process statistics" (pidstats msgq (cfgPidstat cfgBinaries) hostname) 120
  let queue = PQueue.fromList $ catMaybes
                [ Just (now, df)
                , Just (now, mem)
                , (now, ) <$> sk
                , (now, ) <$> p
                -- run two instances of pidstat, each triggered every 2 mins
                -- each one is expected to collect stats for 1 minute
                , Just (now, pidstat1)
                , Just (addUTCTime 60 now, pidstat2)
                ]
  logger <- async $
    -- Pool with just a single connection that closes after 120s of idle
    createPool (Pg.connect cfgDatabase) Pg.close 1 120 1 >>=
      dbLogger msgq shutdown

  runScheduler (Scheduler queue [])

  atomically $ writeTVar shutdown True
  Log.log "Waiting for database logger to complete"
  void $ wait logger

data LoggerResult
  = LoggerContinue
  | LoggerDone

dbLogger
  :: MessageQueue Pg.Pg
  -> TVar Bool
  -> Pool Pg.Connection
  -> IO ()
dbLogger msgq shutdown pool = loop initialBackoff
  where
    loop backoff = do
      state <- try (withResource pool logData)
      case state of
        Left (e :: SomeException) -> do
          Log.warn $ "Database logger raised exception: " <> showTxt e
          Log.log $ "Database logger sleeping " <> showTxt backoff <> " seconds"
          threadDelay (backoff * 1000000)
          flag <- readTVarIO shutdown
          unless flag $ loop (min (backoff * 2) maxBackoff)
        Right LoggerContinue -> loop initialBackoff
        Right LoggerDone -> return ()

    logData conn = do
      action <- atomically $
        (Right <$> recvMessage msgq) `orElse` (Left <$> checkShutdown)
      case action of
        Left () ->
          return LoggerDone
        Right (SampleData insertAction) -> do
          Pg.runBeamPostgresDebug (Log.debug . Text.pack) conn insertAction
          return LoggerContinue

    checkShutdown = check =<< readTVar shutdown

    initialBackoff = 1  -- 1 sec
    maxBackoff = 300    -- 5 min

type InsertValueSyntax cmd = Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)))

diskspace
  :: forall be m. ( BeamSqlBackend be , MonadBeam be m , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) DiskSpaceUsageT )
  => MessageQueue m
  -> FilePath
  -> Text
  -> IO ()
diskspace msgq bin hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (freespace bin hostname now)
  where
    insertAction :: [DiskSpaceUsage] -> m ()
    insertAction = runInsert . insert (dbDiskSpaceUsage db) . insertValues

memory
  :: forall be m. ( BeamSqlBackend be , MonadBeam be m , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) MemoryUsageT )
  => MessageQueue m
  -> FilePath
  -> Text
  -> IO ()
memory msgq bin hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (memoryUsage bin hostname now)
  where
    insertAction :: [MemoryUsage] -> m ()
    insertAction = runInsert . insert (dbMemoryUsage db) . insertValues

sidekiq
  :: forall be m. ( BeamSqlBackend be , MonadBeam be m , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) SidekiqQueueT )
  => MessageQueue m
  -> SidekiqConfig
  -> IO ()
sidekiq msgq SidekiqConfig{..} = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (sidekiqQueues cfgSkQueues cfgSkDatabase now)
  where
    insertAction :: [SidekiqQueue] -> m ()
    insertAction = runInsert . insert (dbSidekiqQueues db) . insertValues

pidstats
  :: forall be m. ( BeamSqlBackend be , MonadBeam be m , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) ProcessStatsT )
  => MessageQueue m
  -> FilePath
  -> Text
  -> IO ()
pidstats msgq bin hostname =
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (processes bin hostname)
  where
    insertAction :: [ProcessStats] -> m ()
    insertAction = runInsert . insert (dbProcessStats db) . insertValues

puma
  :: forall be m. ( BeamSqlBackend be , MonadBeam be m , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) PumaT )
  => MessageQueue m
  -> PumaCtl
  -> Text
  -> IO ()
puma msgq ctl hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (pumaStats ctl hostname now)
  where
    insertAction :: [Puma] -> m ()
    insertAction = runInsert . insert (dbPuma db) . insertValues

runScheduler :: Scheduler -> IO ()
runScheduler scheduler =
  catchJust
    isUserInterrupt
    (go scheduler)
    (const $ Log.log "UserInterrupt")
  where
    go s = do
      (s', delay) <- schedule s
      unless (nothingScheduled s') $ do
        let maxDelay = if null (running s') then 10 else 2
        sleep maxDelay delay
        go s'

    sleep maxDelay t
      | t <= 0 = return ()
      | otherwise = threadDelay $ ceiling (min t maxDelay * 1e6)

    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _ = Nothing

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
          Log.warn $ "Job raised exception: " <> showTxt e
          putMVar mvar False
          return []
        Just (Right ()) -> do
          Log.debug "Job completed"
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
  Log.debug $ "Starting: " <> jobDescription job <> " scheduled for " <> showTxt jobTime
  result <- tryTakeMVar (jobResult job)
  s' <- case result of
    Nothing -> do
      Log.warn "Skipping job, previous instance still busy"
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

showTxt :: Show a => a -> Text
showTxt = Text.pack . show
