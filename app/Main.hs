{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Check.DiskSpaceUsage
  ( DiskSpaceUsage,
    DiskSpaceUsageT,
    freespace,
  )
import Check.Honeybadger
  ( Honeybadger,
    HoneybadgerT,
    honeybadgerFaults,
  )
import Check.Importer (Entry (..), importEntries)
import Check.MemoryUsage (MemoryUsage, MemoryUsageT, memoryUsage)
import Check.Mysql
  ( MysqlProcessList,
    MysqlProcessListT,
    mysqlProcessList,
  )
import Check.ProcessStatistics
  ( ProcessStats,
    ProcessStatsT,
    processes,
  )
import Check.Puma (Puma, PumaCtl (..), PumaT, pumaStats)
import Check.SidekiqQueues
  ( SidekiqQueue,
    SidekiqQueueT,
    sidekiqQueues,
  )
import Control.Applicative (Alternative ((<|>)), (<**>))
import Control.Concurrent
  ( MVar,
    newMVar,
    putMVar,
    threadDelay,
    tryTakeMVar,
  )
import Control.Concurrent.Async (Async, async, poll, wait)
import Control.Concurrent.STM
  ( STM,
    TBQueue,
    TVar,
    atomically,
    check,
    newTBQueue,
    newTVarIO,
    orElse,
    readTBQueue,
    readTVar,
    readTVarIO,
    writeTBQueue,
    writeTVar,
  )
import Control.Error (catMaybes, exceptT)
import Control.Exception
  ( AsyncException (UserInterrupt),
    SomeException,
    catchJust,
    try,
  )
import qualified Control.Logging as Log
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as PQueue
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import Data.Version (showVersion)
import Data.Yaml
  ( FromJSON (parseJSON),
    decodeFileEither,
    prettyPrintParseException,
    withObject,
    withText,
    (.!=),
    (.:),
    (.:?),
  )
import Database
  ( ActionController,
    DB
      ( dbActionController,
        dbBushpigJobs,
        dbDiskSpaceUsage,
        dbFerret,
        dbHoneybadger,
        dbMemoryUsage,
        dbMysqlProcesslist,
        dbProcessStats,
        dbPuma,
        dbSidekiqJobs,
        dbSidekiqQueues
      ),
    SidekiqJob,
    SidekiqJobT
      ( SidekiqJobT,
        sjClass,
        sjCompletedAt,
        sjEnqueuedAt,
        sjJobId,
        sjParams,
        sjQueue,
        sjStartedAt
      ),
    BushpigJob,
    BushpigJobT
      ( BushpigJobT,
        bjClass,
        bjCompletedAt,
        bjEnqueuedAt,
        bjHost,
        bjJobId,
        bjJobKey,
        bjParams,
        bjStartedAt
      ),
    Ferret,
    FerretT
      ( FerretT,
        fNodeId,
        fParentId,
        fHost,
        fLabel,
        fContext,
        fEnteredAt,
        fLeftAt
      ),
    db,
  )
import Database.Beam
  ( MonadBeam,
    SqlJustable (just_, nothing_),
    coalesce_,
    current_,
    insert,
    insertValues,
    runInsert,
    (<-.),
  )
import Database.Beam.Backend.SQL
  ( BeamSqlBackend,
    BeamSqlBackendCanSerialize,
    IsSql92ExpressionSyntax (Sql92ExpressionValueSyntax),
    IsSql92InsertSyntax (Sql92InsertValuesSyntax),
    IsSql92InsertValuesSyntax (Sql92InsertValuesExpressionSyntax),
    IsSql92Syntax (Sql92InsertSyntax),
  )
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Full as Pg
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import qualified Database.Redis as Redis
import qualified Network.HTTP.Req as Req
import Network.HostName (getHostName)
import qualified Options.Applicative as OptParse
import Paths_meerkat (version)

data Config = Config
  { cfgHostname :: Maybe Text,
    cfgLogging :: LogConfig,
    cfgDatabase :: Pg.ConnectInfo,
    cfgBinaries :: BinConfig,
    cfgSidekiq :: Maybe SidekiqConfig,
    cfgPuma :: Maybe PumaConfig,
    cfgHoneybadger :: Maybe HoneybadgerConfig,
    cfgImporter :: Maybe ImporterConfig
  }

data BinConfig = BinConfig
  { cfgDF :: FilePath,
    cfgFree :: FilePath,
    cfgPidstat :: FilePath,
    cfgMysql :: Maybe FilePath
  }

defaultBinConfig :: BinConfig
defaultBinConfig = BinConfig "df" "free" "pidstat" Nothing

data LogConfig = LogConfig
  { cfgLogFile :: Maybe FilePath,
    cfgLogLevel :: Log.LogLevel
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig Nothing Log.LevelInfo

data SidekiqConfig = SidekiqConfig
  { cfgSkDatabases :: [Redis.ConnectInfo]
  }

data PumaConfig = PumaConfig
  { cfgPumaCtl :: PumaCtl
  }

data HoneybadgerConfig = HoneybadgerConfig
  { cfgHBProjectId :: Integer,
    cfgHBEnvironment :: Text,
    cfgHBAuthToken :: ByteString
  }

data ImporterConfig = ImporterConfig
  { cfgIPath :: FilePath
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config
      <$> o .:? "hostname"
      <*> o .:? "logging" .!= defaultLogConfig
      <*> o .: "database"
      <*> o .:? "binaries" .!= defaultBinConfig
      <*> o .:? "sidekiq"
      <*> o .:? "puma"
      <*> o .:? "honeybadger"
      <*> o .:? "importer"

instance FromJSON LogConfig where
  parseJSON = withObject "LogConfig" $ \o ->
    LogConfig
      <$> o .:? "logfile" .!= cfgLogFile defaultLogConfig
      <*> o .:? "loglevel" .!= cfgLogLevel defaultLogConfig

instance FromJSON BinConfig where
  parseJSON = withObject "BinConfig" $ \o ->
    BinConfig
      <$> o .:? "df" .!= cfgDF defaultBinConfig
      <*> o .:? "free" .!= cfgFree defaultBinConfig
      <*> o .:? "pidstat" .!= cfgPidstat defaultBinConfig
      <*> o .:? "mysql"

instance FromJSON SidekiqConfig where
  parseJSON = withObject "SidekiqConfig" $ \o ->
    SidekiqConfig
      <$> o .: "databases"

instance FromJSON PumaConfig where
  parseJSON = withObject "PumaConfig" $ \o -> do
    PumaCtl url opt <- o .: "url"
    token <- o .: "token"
    return $ PumaConfig (PumaCtl url (opt <> "token" Req.=: (token :: Text)))

instance FromJSON HoneybadgerConfig where
  parseJSON = withObject "HoneybadgerConfig" $ \o ->
    HoneybadgerConfig
      <$> o .: "project_id"
      <*> o .: "environment"
      <*> (Text.encodeUtf8 <$> o .: "auth_token")

instance FromJSON ImporterConfig where
  parseJSON = withObject "ImporterConfig" $ \o ->
    ImporterConfig
      <$> o .: "path"

-- Orphan
instance FromJSON Pg.ConnectInfo where
  parseJSON = withObject "ConnectInfo" $ \o ->
    Pg.ConnectInfo
      <$> o .:? "host" .!= Pg.connectHost Pg.defaultConnectInfo
      <*> o .:? "port" .!= Pg.connectPort Pg.defaultConnectInfo
      <*> o .:? "user" .!= "meerkat"
      <*> o .:? "password" .!= Pg.connectPassword Pg.defaultConnectInfo
      <*> o .:? "database" .!= "meerkat"

-- Orphan
instance FromJSON Redis.ConnectInfo where
  parseJSON =
    withText "ConnectInfo" $
      either fail return . Redis.parseConnectInfo . Text.unpack

-- Orphan
instance FromJSON Log.LogLevel where
  parseJSON = withText "LogLevel" parseLogLevel
    where
      parseLogLevel "debug" = return Log.LevelDebug
      parseLogLevel "info" = return Log.LevelInfo
      parseLogLevel "warn" = return Log.LevelWarn
      parseLogLevel "error" = return Log.LevelError
      parseLogLevel txt = fail $ "'" <> Text.unpack txt <> "' is not a valid log level"

newtype MeerkatOptions = MeerkatOptions
  { configFile :: FilePath
  }

meerkatOptions :: OptParse.Parser MeerkatOptions
meerkatOptions =
  MeerkatOptions
    <$> OptParse.strOption
      ( OptParse.long "config-file"
          <> OptParse.metavar "PATH"
          <> OptParse.short 'c'
          <> OptParse.value "meerkat.yaml"
          <> OptParse.help "Path to Meerkat configuration file"
      )

data Job = Job
  { jobDescription :: Text,
    jobAction :: IO (),
    jobInterval :: Maybe NominalDiffTime,
    jobResult :: MVar Bool
  }

newPeriodicJob :: Text -> IO () -> NominalDiffTime -> IO Job
newPeriodicJob desc action interval = do
  result <- newMVar True
  return
    Job
      { jobDescription = desc,
        jobAction = action,
        jobInterval = Just interval,
        jobResult = result
      }

newStreamingJob :: Text -> IO () -> IO Job
newStreamingJob desc action = do
  result <- newMVar True
  return
    Job
      { jobDescription = desc,
        jobAction = action,
        jobInterval = Nothing,
        jobResult = result
      }

type JobQueue = PQueue.MinPQueue UTCTime Job

data Running = Running (Async ()) (MVar Bool)

data Scheduler = Scheduler
  { queue :: !JobQueue,
    running :: ![Running]
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
    opts =
      OptParse.info
        (meerkatOptions <**> OptParse.helper)
        ( OptParse.fullDesc <> OptParse.progDesc "Meerkat"
            <> OptParse.header
              "Meerkat - monitors your system"
        )

    initialise MeerkatOptions {..} =
      decodeFileEither configFile >>= either parseError runApp

    parseError e = do
      putStrLn "Unable to parse configuration file"
      putStrLn $ prettyPrintParseException e

    runApp cfg@Config {..} = do
      let withLogging = maybe Log.withStdoutLogging Log.withFileLogging (cfgLogFile cfgLogging)
      withLogging $ do
        Log.setLogLevel (cfgLogLevel cfgLogging)
        app cfg

app :: Config -> IO ()
app Config {..} = do
  msgq <- atomically $ newTBQueue 1000
  shutdown <- newTVarIO False
  hostname <- maybe (Text.pack <$> getHostName) return cfgHostname
  now <- getCurrentTime
  Log.log $
    Text.unwords
      [ "Meerkat",
        Text.pack (showVersion version),
        "started on",
        hostname,
        "at",
        showTxt now
      ]

  df <- newPeriodicJob "Check disk space usage" (diskspace msgq (cfgDF cfgBinaries) hostname) 60
  mem <- newPeriodicJob "Check memory usage" (memory msgq (cfgFree cfgBinaries) hostname) 20
  p <- traverse (\cfg -> newPeriodicJob "Poll Puma statistics" (puma msgq (cfgPumaCtl cfg) hostname) 20) cfgPuma
  hb <- traverse (\cfg -> newPeriodicJob "Poll Honeybadger" (honeybadger msgq cfg) 600) cfgHoneybadger
  i <- traverse (\cfg -> newPeriodicJob "Importer" (importer msgq cfg) 60) cfgImporter
  pidstat1 <- newPeriodicJob "Check process statistics" (pidstats msgq (cfgPidstat cfgBinaries) hostname) 120
  pidstat2 <- newPeriodicJob "Check process statistics" (pidstats msgq (cfgPidstat cfgBinaries) hostname) 120
  my <- traverse (\bin -> newPeriodicJob "MySql processlist" (mysql msgq bin hostname) 60) (cfgMysql cfgBinaries)
  sk <- traverse (\cfg -> newPeriodicJob "Check sidekiq queues" (sidekiq msgq cfg) 60) cfgSidekiq
  let queue =
        PQueue.fromList $
          catMaybes
            [ Just (now, df),
              Just (now, mem),
              (now,) <$> sk,
              (now,) <$> p,
              (now,) <$> hb,
              (now,) <$> i,
              (now,) <$> my,
              -- run two instances of pidstat, each triggered every 2 mins
              -- each one is expected to collect stats for 1 minute
              Just (now, pidstat1),
              Just (addUTCTime 60 now, pidstat2)
            ]
  logger <-
    async $
      -- Pool with just a single connection that closes after 120s of idle
      createPool (Pg.connect cfgDatabase) Pg.close 1 120 1
        >>= dbLogger msgq shutdown

  runScheduler (Scheduler queue [])

  atomically $ writeTVar shutdown True
  Log.log "Waiting for database logger to complete"
  void $ wait logger

data LoggerResult
  = LoggerContinue
  | LoggerDone

dbLogger ::
  MessageQueue Pg.Pg ->
  TVar Bool ->
  Pool Pg.Connection ->
  IO ()
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
      action <-
        atomically $
          (Right <$> recvMessage msgq) `orElse` (Left <$> checkShutdown)
      case action of
        Left () ->
          return LoggerDone
        Right (SampleData insertAction) -> do
          Pg.runBeamPostgresDebug (Log.debug . Text.pack) conn insertAction
          return LoggerContinue

    checkShutdown = check =<< readTVar shutdown

    initialBackoff = 1 -- 1 sec
    maxBackoff = 300 -- 5 min

type InsertValueSyntax cmd = Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)))

diskspace ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) DiskSpaceUsageT) =>
  MessageQueue m ->
  FilePath ->
  Text ->
  IO ()
diskspace msgq bin hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (freespace bin hostname now)
  where
    insertAction :: [DiskSpaceUsage] -> m ()
    insertAction = runInsert . insert (dbDiskSpaceUsage db) . insertValues

memory ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) MemoryUsageT) =>
  MessageQueue m ->
  FilePath ->
  Text ->
  IO ()
memory msgq bin hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (memoryUsage bin hostname now)
  where
    insertAction :: [MemoryUsage] -> m ()
    insertAction = runInsert . insert (dbMemoryUsage db) . insertValues

sidekiq ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) SidekiqQueueT) =>
  MessageQueue m ->
  SidekiqConfig ->
  IO ()
sidekiq msgq SidekiqConfig {..} = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (sidekiqQueues cfgSkDatabases now)
  where
    insertAction :: [SidekiqQueue] -> m ()
    insertAction = runInsert . insert (dbSidekiqQueues db) . insertValues

pidstats ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) ProcessStatsT) =>
  MessageQueue m ->
  FilePath ->
  Text ->
  IO ()
pidstats msgq bin hostname =
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (processes bin hostname)
  where
    insertAction :: [ProcessStats] -> m ()
    insertAction = runInsert . insert (dbProcessStats db) . insertValues

puma ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) PumaT) =>
  MessageQueue m ->
  PumaCtl ->
  Text ->
  IO ()
puma msgq ctl hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (pumaStats ctl hostname now)
  where
    insertAction :: [Puma] -> m ()
    insertAction = runInsert . insert (dbPuma db) . insertValues

honeybadger ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) HoneybadgerT) =>
  MessageQueue m ->
  HoneybadgerConfig ->
  IO ()
honeybadger msgq HoneybadgerConfig {..} = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (honeybadgerFaults cfgHBProjectId cfgHBEnvironment cfgHBAuthToken now)
  where
    insertAction :: [Honeybadger] -> m ()
    insertAction = runInsert . insert (dbHoneybadger db) . insertValues

mysql ::
  forall be m.
  (BeamSqlBackend be, MonadBeam be m, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) MysqlProcessListT) =>
  MessageQueue m ->
  FilePath ->
  Text ->
  IO ()
mysql msgq bin hostname = do
  now <- getCurrentTime
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (mysqlProcessList bin hostname now)
  where
    insertAction :: [MysqlProcessList] -> m ()
    insertAction = runInsert . insert (dbMysqlProcesslist db) . insertValues

groupEntries :: [Entry] -> ([ActionController], [SidekiqJob], [BushpigJob], [Ferret])
groupEntries entries =
  let (acs, sjs, bjs, fs) = go entries in (acs, dedupSJ sjs, dedupBJ bjs, dedupF fs)
  where
    go es = mconcat (map byGroup es)
    byGroup (ActionControllerEntry x) = ([x], [], [], [])
    byGroup (SidekiqJobEntry x) = ([], [x], [], [])
    byGroup (BushpigJobEntry x) = ([], [], [x], [])
    byGroup (FerretEntry x) = ([], [], [], [x])
    dedupSJ = HM.elems . HM.fromListWith sjcoalesce . map (\sj -> (sjJobId sj, sj))
    dedupBJ = HM.elems . HM.fromListWith bjcoalesce . map (\bj -> (bjJobId bj, bj))
    dedupF = HM.elems . HM.fromListWith fcoalesce . map (\f -> (fNodeId f, f))
    sjcoalesce a b =
      SidekiqJobT
        { sjJobId = sjJobId b,
          sjQueue = sjQueue b,
          sjClass = sjClass b,
          sjParams = sjParams b,
          sjEnqueuedAt = sjEnqueuedAt b,
          sjStartedAt = sjStartedAt b <|> sjStartedAt a,
          sjCompletedAt = sjCompletedAt b <|> sjCompletedAt a
        }
    bjcoalesce a b =
      BushpigJobT
        { bjJobId = bjJobId b,
          bjJobKey = bjJobKey b,
          bjHost = bjHost b,
          bjClass = bjClass b,
          bjParams = bjParams b,
          bjEnqueuedAt = bjEnqueuedAt b <|> bjEnqueuedAt a,
          bjStartedAt = bjStartedAt b <|> bjStartedAt a,
          bjCompletedAt = bjCompletedAt b <|> bjCompletedAt a
        }
    fcoalesce a b =
      FerretT
        { fNodeId = fNodeId b,
          fParentId = fParentId b <|> fParentId a,
          fHost = fHost b <|> fHost a,
          fLabel = fLabel b <|> fLabel a,
          fContext = fContext b <|> fContext a,
          fEnteredAt = fEnteredAt b <|> fEnteredAt a,
          fLeftAt = fLeftAt b <|> fLeftAt a
        }

importer ::
  MessageQueue Pg.Pg ->
  ImporterConfig ->
  IO ()
importer msgq ImporterConfig {..} =
  exceptT
    print
    (atomically . sendMessage msgq . SampleData . insertAction)
    (importEntries cfgIPath)
  where
    insertAction :: [Entry] -> Pg.Pg ()
    insertAction entries = do
      let (ac, sj, bj, f) = groupEntries entries
      unless (null ac) $ insertActionControllerEntries ac
      unless (null sj) $ insertSidekiqJobEntries sj
      unless (null bj) $ insertBushpigJobEntries bj
      unless (null f) $ insertFerretEntries f

    insertActionControllerEntries :: [ActionController] -> Pg.Pg ()
    insertActionControllerEntries es =
      runInsert $
        insert (dbActionController db) $
          insertValues es

    insertSidekiqJobEntries :: [SidekiqJob] -> Pg.Pg ()
    insertSidekiqJobEntries es =
      runInsert $
        Pg.insert
          (dbSidekiqJobs db)
          (insertValues es)
          ( Pg.onConflict
              (Pg.conflictingFields sjJobId)
              ( Pg.onConflictUpdateSet
                  ( \tbl tblExcl ->
                      mconcat
                        [ sjQueue tbl <-. sjQueue tblExcl,
                          sjClass tbl <-. sjClass tblExcl,
                          sjParams tbl <-. sjParams tblExcl,
                          sjEnqueuedAt tbl <-. sjEnqueuedAt tblExcl,
                          sjStartedAt tbl <-. coalesce_ [just_ (sjStartedAt tblExcl), just_ (current_ (sjStartedAt tbl))] nothing_,
                          sjCompletedAt tbl <-. coalesce_ [just_ (sjCompletedAt tblExcl), just_ (current_ (sjCompletedAt tbl))] nothing_
                        ]
                  )
              )
          )

    insertBushpigJobEntries :: [BushpigJob] -> Pg.Pg ()
    insertBushpigJobEntries es =
      runInsert $
        Pg.insert
          (dbBushpigJobs db)
          (insertValues es)
          ( Pg.onConflict
              (Pg.conflictingFields bjJobId)
              ( Pg.onConflictUpdateSet
                  ( \tbl tblExcl ->
                      mconcat
                        [ bjJobId tbl <-. bjJobId tblExcl,
                          bjJobKey tbl <-. bjJobKey tblExcl,
                          bjHost tbl <-. bjHost tblExcl,
                          bjClass tbl <-. bjClass tblExcl,
                          bjParams tbl <-. bjParams tblExcl,
                          bjEnqueuedAt tbl <-. coalesce_ [just_ (bjEnqueuedAt tblExcl), just_ (current_ (bjEnqueuedAt tbl))] nothing_,
                          bjStartedAt tbl <-. coalesce_ [just_ (bjStartedAt tblExcl), just_ (current_ (bjStartedAt tbl))] nothing_,
                          bjCompletedAt tbl <-. coalesce_ [just_ (bjCompletedAt tblExcl), just_ (current_ (bjCompletedAt tbl))] nothing_
                        ]
                  )
              )
          )

    insertFerretEntries :: [Ferret] -> Pg.Pg ()
    insertFerretEntries fs =
      runInsert $
        Pg.insert
          (dbFerret db)
          (insertValues fs)
          ( Pg.onConflict
              (Pg.conflictingFields fNodeId)
              ( Pg.onConflictUpdateSet
                  ( \tbl tblExcl ->
                      mconcat
                        [ fNodeId tbl <-. fNodeId tblExcl,
                          fParentId tbl <-. coalesce_ [just_ (fParentId tblExcl), just_ (current_ (fParentId tbl))] nothing_,
                          fHost tbl <-. coalesce_ [just_ (fHost tblExcl), just_ (current_ (fHost tbl))] nothing_,
                          fLabel tbl <-. coalesce_ [just_ (fLabel tblExcl), just_ (current_ (fLabel tbl))] nothing_,
                          fContext tbl <-. coalesce_ [just_ (fContext tblExcl), just_ (current_ (fContext tbl))] nothing_,
                          fEnteredAt tbl <-. coalesce_ [just_ (fEnteredAt tblExcl), just_ (current_ (fEnteredAt tbl))] nothing_,
                          fLeftAt tbl <-. coalesce_ [just_ (fLeftAt tblExcl), just_ (current_ (fLeftAt tbl))] nothing_
                        ]
                  )
              )
          )

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
  return Scheduler {running = running', ..}
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
        then return (s, diffUTCTime jobTime now)
        else start s jobTime job >>= sow now

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
      return s {running = Running a (jobResult job) : running}
  return s' {queue = reschedule jobTime job queue}

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
