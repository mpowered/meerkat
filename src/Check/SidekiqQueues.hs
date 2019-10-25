{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.SidekiqQueues
  ( SidekiqQueue
  , SidekiqQueueT (..)
  , sidekiqQueues
  , sidekiqJobs
  )
where

import           Control.Error
import           Control.Exception
import           Data.Aeson                 as Aeson
import qualified Data.HashMap.Strict        as HM
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime, NominalDiffTime, diffUTCTime)
import           Data.Time.Clock.POSIX      (POSIXTime, posixSecondsToUTCTime)
import           Database
import qualified Database.Redis             as Redis

type Class = Text

newtype JobsStats = JobsStats (HM.HashMap Class ClassStats)

instance Semigroup JobsStats where
  JobsStats a <> JobsStats b = JobsStats $ HM.unionWith (<>) a b

instance Monoid JobsStats where
  mempty = JobsStats HM.empty

data ClassStats = ClassStats
  { classNumJobs    :: Integer
  , classTotalAge   :: NominalDiffTime
  }

instance Semigroup ClassStats where
  a <> b = ClassStats (classNumJobs a + classNumJobs b) (classTotalAge a + classTotalAge b)

instance Monoid ClassStats where
  mempty = ClassStats 0 0

data Job = Job
  { jobClass        :: Class
  -- , jobArgs         :: Value
  -- , jobCreatedAt    :: UTCTime
  , jobEnqueuedAt   :: UTCTime
  }
  
instance FromJSON Job where
  parseJSON = withObject "SidekiqJob" $ \o ->
    Job
      <$> o .: "class"
      -- <*> o .: "args"
      -- <*> o .: "created_at"
      <*> (parseDate <$> o .: "enqueued_at")
    where
      parseDate = posixSecondsToUTCTime . (realToFrac :: Double -> POSIXTime) . read

runRedis :: Redis.Connection -> Redis.Redis (Either Redis.Reply a) -> ExceptT String IO a
runRedis conn r = withExceptT redisResult $ ExceptT $ Redis.runRedis conn r
  where
    redisResult (Redis.Error msg) = "Redis error: " ++ show msg
    redisResult _                 = "Redis returned unexpected result"

withConn :: Redis.ConnectInfo -> (Redis.Connection -> ExceptT String IO a) -> ExceptT String IO a
withConn conninfo a = do
  e <- withExceptT show $ tryIO $
    bracket (Redis.checkedConnect conninfo) Redis.disconnect (runExceptT . a)
  hoistEither e

sidekiqQueues :: Text -> Redis.ConnectInfo -> UTCTime -> ExceptT String IO [SidekiqQueue]
sidekiqQueues env conninfo timestamp =
  withConn conninfo $ \conn -> do
    names <- runRedis conn $ Redis.smembers queues
    let qnames = map queue names
    qlens <- runRedis conn $ sequence <$> mapM Redis.llen qnames
    return $ zipWith (SidekiqQueueT timestamp) (Text.decodeUtf8 <$> qnames) qlens
  where
    queues = Text.encodeUtf8 $ env <> ":queues"
    queue n = Text.encodeUtf8 env <> ":queue:" <> n

sidekiqJobs :: Text -> Redis.ConnectInfo -> UTCTime -> ExceptT String IO [SidekiqJobs]
sidekiqJobs env conninfo timestamp =
  withConn conninfo $ \conn -> do
    names <- runRedis conn $ Redis.smembers queues
    let qnames = map queue names
    jobs <- runRedis conn $ sequence <$> mapM (\q -> Redis.lrange q 0 (-1)) qnames
    let JobsStats stats = mconcat $ map (maybe mempty (jobStats timestamp) . Aeson.decodeStrict') (concat jobs)
    return $ map (uncurry mkSidekiqJob) (HM.toList stats)
  where
    queues = Text.encodeUtf8 $ env <> ":queues"
    queue n = Text.encodeUtf8 env <> ":queue:" <> n

    mkSidekiqJob cls stat =
      SidekiqJobsT
        { sjTime = timestamp
        , sjClass = cls
        , sjLength = classNumJobs stat
        , sjEnqueuedFor = realToFrac (classTotalAge stat)
        }

jobStats :: UTCTime -> Job -> JobsStats
jobStats now j = JobsStats $ HM.singleton (jobClass j) (ClassStats 1 (diffUTCTime now (jobEnqueuedAt j)))