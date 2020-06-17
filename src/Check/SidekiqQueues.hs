{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.SidekiqQueues
  ( SidekiqQueue
  , SidekiqQueueT (..)
  , sidekiqQueues
  )
where

import           Control.Error
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Catch        as Catch
import           Data.Aeson                 as Aeson
import qualified Data.HashMap.Strict        as HM
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime, NominalDiffTime, diffUTCTime)
import           Data.Time.Clock.POSIX      (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Vector                as Vec
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
  , classJobs       :: [Text]
  }

instance Semigroup ClassStats where
  a <> b = ClassStats (classNumJobs a + classNumJobs b)
                      (classTotalAge a + classTotalAge b)
                      (classJobs a <> classJobs b)

instance Monoid ClassStats where
  mempty = ClassStats 0 0 []

data Job = Job
  { jobClass        :: Text
  , jobId           :: Text
  , jobEnqueuedAt   :: UTCTime
  }

instance FromJSON Job where
  parseJSON = withObject "SidekiqJob" $ \o ->
    Job
      <$> o .: "class"
      <*> o .: "jid"
      <*> (o .: "enqueued_at" >>= parseDate)
    where
      parseDate (Aeson.String s) = return $ posixSecondsToUTCTime $ (realToFrac :: Double -> POSIXTime) $ read $ Text.unpack s
      parseDate (Aeson.Number n) = return $ posixSecondsToUTCTime $ realToFrac n
      parseDate _                = error "Not a valid date"

runRedis :: Redis.Connection -> Redis.Redis (Either Redis.Reply a) -> ExceptT String IO a
runRedis conn r = withExceptT redisResult $ ExceptT $ Redis.runRedis conn r
  where
    redisResult (Redis.Error msg) = "Redis error: " ++ show msg
    redisResult _                 = "Redis returned unexpected result"

sidekiqQueues ::  [Redis.ConnectInfo] -> UTCTime -> ExceptT String IO [SidekiqQueue]
sidekiqQueues databases timestamp = concat <$> mapM go databases
  where
    -- in newer hedis
    withConnect conninfo = Catch.bracket (liftIO $ Redis.connect conninfo) (liftIO . Redis.disconnect)
    go conninfo = withConnect conninfo $ \conn -> do
      names <- runRedis conn $ Redis.smembers queues
      let qnames = map queue names
      jobs <- runRedis conn $ sequence <$> mapM (\q -> Redis.lrange q 0 (-1)) qnames
      let JobsStats stats =
            mconcat [ maybe mempty (jobStats timestamp q) (Aeson.decodeStrict' j)
                    | (q, js) <- zip (Text.decodeUtf8 <$> qnames) jobs
                    , j <- js ]
      return $ map (uncurry mkSidekiqJob) (HM.toList stats)

    queues = "queues"
    queue n = "queue:" <> n

    mkSidekiqJob (q,cls) stat =
      SidekiqQueueT
        { sqTime = timestamp
        , sqQueue = q
        , sqClass = cls
        , sqLength = classNumJobs stat
        , sqEnqueuedFor = realToFrac (classTotalAge stat)
        , sqJobIds = Vec.fromList (classJobs stat)
        }

jobStats :: UTCTime -> Text -> Job -> JobsStats
jobStats now q j = JobsStats $ HM.singleton (q,jobClass j) (ClassStats 1 (diffUTCTime now (jobEnqueuedAt j)) [jobId j])
