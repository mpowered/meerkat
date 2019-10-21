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
import           Control.Exception
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime)
import           Database
import qualified Database.Redis             as Redis

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
