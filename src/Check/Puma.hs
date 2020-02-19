{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.Puma
  ( Puma
  , PumaT (..)
  , PumaCtl (..)
  , pumaStats
  )
where

import           Control.Error
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime)
import           Data.Yaml
import           Database
import           Network.HTTP.Req

data PumaCtl = forall scheme. PumaCtl (Url scheme) (Option scheme)

instance FromJSON PumaCtl where
  parseJSON = withText "PumaCtlUrl" $ \s ->
    case parseUrl (Text.encodeUtf8 s) of
      Nothing -> fail $ "'" <> Text.unpack s <> "' is not a valid url"
      Just (Left  (url, option)) -> return $ PumaCtl url option
      Just (Right (url, option)) -> return $ PumaCtl url option

data PumaStats = PumaStats
  { statsWorkers      :: Integer
  , statsWorkerStatus :: [WorkerStatus]
  }

instance FromJSON PumaStats where
  parseJSON = withObject "PumaStats" $ \o ->
    PumaStats
      <$> o .: "workers"
      <*> o .: "worker_status"

data WorkerStatus = WorkerStatus
  { workerIndex       :: Integer
  , workerStatus      :: Maybe Status
  }

instance FromJSON WorkerStatus where
  parseJSON = withObject "WorkerStatus" $ \o ->
    WorkerStatus
      <$> o .: "index"
      <*> o .: "last_status"

data Status = Status
  { statusBacklog       :: Integer
  , statusRunning       :: Integer
  , statusPoolCapacity  :: Integer
  , statusMaxThreads    :: Integer
  }

instance FromJSON Status where
  parseJSON = withObject "Status" $ \o ->
    Status
      <$> o .: "backlog"
      <*> o .: "running"
      <*> o .: "pool_capacity"
      <*> o .: "max_threads"

pumaStats :: PumaCtl -> Text.Text -> UTCTime -> ExceptT String IO [Puma]
pumaStats (PumaCtl url opts) host timestamp = do
  stats <- runReq defaultHttpConfig $
    req GET url NoReqBody jsonResponse opts
  return $ concatMap extractStats (statsWorkerStatus $ responseBody stats)
  where
    extractStats w =
      case workerStatus w of
        Just status -> [ PumaT { pTime          = timestamp
                               , pHost          = host
                               , pWorker        = workerIndex w
                               , pBacklog       = statusBacklog status
                               , pRunning       = statusRunning status
                               , pPoolCapacity  = statusPoolCapacity status
                               , pMaxThreads    = statusMaxThreads status
                               }
                       ]
        Nothing -> []