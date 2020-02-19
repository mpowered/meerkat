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
import qualified Control.Logging          as Log
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8    as C8
import           Data.List                (sort)
import qualified Data.Text                as Text
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

importFile :: FilePath -> IO [Entry]
importFile path = do
  c <- C8.readFile path
  let es = map eitherDecodeStrict (C8.lines c)
  removeFile path
  let (errs, entries) = partitionEithers es
  unless (null errs) $ do
    Log.warn $ Text.pack (show $ length errs) <> " parse errors importing " <> Text.pack path
    mapM_ (Log.debug . Text.pack) (take 5 errs)
  Log.debug $ "Imported " <> Text.pack (show $ length entries) <> " entries from " <> Text.pack path
  return entries

importEntries :: FilePath -> ExceptT String IO [Entry]
importEntries path =
  liftIO $ do
    dirEntries <- map (path </>) <$> listDirectory path
    files <- filterM doesFileExist dirEntries
    mtimes <- mapM getModificationTime files
    -- sort on (mtime, filename) so filename splits ties in mtime
    let sorted = map snd $ sort $ zip mtimes files
    concat <$> mapM importFile sorted
