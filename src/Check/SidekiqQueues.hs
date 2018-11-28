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
import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime)
import           Database
import qualified Database.Redis             as Redis

sidekiqQueues :: [Text] -> Redis.ConnectInfo -> UTCTime -> ExceptT String IO [SidekiqQueue]
sidekiqQueues queues conninfo timestamp = do
  results <- catchConnectionLost $ do
    bracket (Redis.checkedConnect conninfo) (Redis.disconnect) $ \conn ->
      Redis.runRedis conn $ do
        mapM Redis.llen (Text.encodeUtf8 <$> queues)
  case sequence results of
    Left reply -> throwE $ "Redis returned reply: " ++ show reply
    Right qlens -> return $ zipWith (SidekiqQueueT timestamp) queues qlens
  where
    catchConnectionLost :: IO a -> ExceptT String IO a
    catchConnectionLost action = do
      r <- liftIO $ try action
      case r of
        Left Redis.ConnectionLost -> throwE "Redis connection lost"
        Right a -> return a
