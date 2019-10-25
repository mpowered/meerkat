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

type QueueClass = (Text,Text)

newtype JobsStats = JobsStats (HM.HashMap QueueClass ClassStats)

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
  { jobClass        :: Text
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
    let JobsStats stats =
          mconcat [ maybe mempty (jobStats timestamp q) (Aeson.decodeStrict' j)
                  | (q, js) <- zip (Text.decodeUtf8 <$> qnames) jobs
                  , j <- js ]
    return $ map (uncurry mkSidekiqJob) (HM.toList stats)
  where
    queues = Text.encodeUtf8 $ env <> ":queues"
    queue n = Text.encodeUtf8 env <> ":queue:" <> n

    mkSidekiqJob (q,cls) stat =
      SidekiqJobsT
        { sjTime = timestamp
        , sjQueue = q
        , sjClass = cls
        , sjLength = classNumJobs stat
        , sjEnqueuedFor = realToFrac (classTotalAge stat)
        }

jobStats :: UTCTime -> Text -> Job -> JobsStats
jobStats now q j = JobsStats $ HM.singleton (q,jobClass j) (ClassStats 1 (diffUTCTime now (jobEnqueuedAt j)))