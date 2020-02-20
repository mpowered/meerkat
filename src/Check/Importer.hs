{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.Importer
  ( Entry(..)
  , importEntries
  )
where

import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.List                (sort)
import           Database
import           System.Directory
import           System.FilePath

data Entry
  = ActionControllerEntry ActionController
  | SidekiqJobEntry SidekiqJob

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \e -> do
    ty <- e .: "type"
    case ty of
      "action_controller"   -> actionController e
      "sidekiq_job"         -> sidekiqJob e
      invalid               -> fail ("unexpected Entry type " <> invalid)
    where
      actionController e = ActionControllerEntry <$>
        ( ActionControllerT
          <$> e .:  "time"
          <*> e .:  "host"
          <*> e .:  "app"
          <*> e .:? "user_id"
          <*> e .:? "account_id"
          <*> e .:? "scorecard_id"
          <*> e .:  "controller"
          <*> e .:  "action"
          <*> e .:? "params"
          <*> e .:? "format"
          <*> e .:  "method"
          <*> e .:  "path"
          <*> e .:? "status"
          <*> e .:? "view_runtime"
          <*> e .:? "db_runtime"
          <*> e .:? "total_runtime"
        )
      sidekiqJob e = SidekiqJobEntry <$>
        ( SidekiqJobT
          <$> e .:  "jid"
          <*> e .:  "queue"
          <*> e .:  "class"
          <*> e .:  "params"
          <*> e .:  "enqueued_at"
          <*> e .:? "started_at"
          <*> e .:? "completed_at"
        )

importFile :: FilePath -> ExceptT String IO [Entry]
importFile path = do
  entries <- ExceptT $ eitherDecodeFileStrict' path
  liftIO $ removeFile path
  return entries

importEntries :: FilePath -> ExceptT String IO [Entry]
importEntries path = do
  sorted <- liftIO $ do
    dirEntries <- map (path </>) <$> listDirectory path
    files <- filter ("json" `isExtensionOf`) <$> filterM doesFileExist dirEntries
    mtimes <- mapM getModificationTime files
    -- sort on (mtime, filename) so filename splits ties in mtime
    return $ map snd $ sort $ zip mtimes files
  concat <$> mapM importFile sorted
