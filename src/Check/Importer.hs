{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Check.Importer
  ( Entry (..),
    importEntries,
  )
where

import Control.Error (ExceptT (..))
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value,
    eitherDecodeFileStrict',
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Extra.Recursive (ValueF (StringF))
import Data.Functor.Foldable
  ( Corecursive (embed),
    Recursive (cata),
  )
import Data.List (sort)
import qualified Data.Text as T
import Database
  ( ActionController,
    ActionControllerT (ActionControllerT),
    SidekiqJob,
    SidekiqJobT (SidekiqJobT),
    BushpigJob,
    BushpigJobT (BushpigJobT),
  )
import System.Directory
  ( doesFileExist,
    getModificationTime,
    listDirectory,
    removeFile,
  )
import System.FilePath (isExtensionOf, (</>))

data Entry
  = ActionControllerEntry ActionController
  | SidekiqJobEntry SidekiqJob
  | BushpigJobEntry BushpigJob

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \e -> do
    ty <- e .: "type"
    case ty of
      "action_controller" -> actionController e
      "sidekiq_job" -> sidekiqJob e
      "bushpig_job" -> bushpigJob e
      invalid -> fail ("unexpected Entry type " <> invalid)
    where
      actionController e =
        ActionControllerEntry
          <$> ( ActionControllerT
                  <$> e .: "time"
                  <*> e .: "host"
                  <*> e .: "app"
                  <*> e .:? "user_id"
                  <*> e .:? "account_id"
                  <*> e .:? "scorecard_id"
                  <*> e .: "controller"
                  <*> e .: "action"
                  <*> (fmap clean <$> (e .:? "params"))
                  <*> e .:? "format"
                  <*> e .: "method"
                  <*> e .: "path"
                  <*> e .:? "status"
                  <*> e .:? "view_runtime"
                  <*> e .:? "db_runtime"
                  <*> e .:? "total_runtime"
                  <*> e .:? "headers"
              )
      sidekiqJob e =
        SidekiqJobEntry
          <$> ( SidekiqJobT
                  <$> e .: "jid"
                  <*> e .: "queue"
                  <*> e .: "class"
                  <*> (clean <$> (e .: "params"))
                  <*> e .: "enqueued_at"
                  <*> e .:? "started_at"
                  <*> e .:? "completed_at"
              )
      bushpigJob e =
        BushpigJobEntry
          <$> ( BushpigJobT
                  <$> e .: "jid"
                  <*> e .: "jkey"
                  <*> e .: "host"
                  <*> e .: "class"
                  <*> (clean <$> (e .: "params"))
                  <*> e .:? "enqueued_at"
                  <*> e .:? "started_at"
                  <*> e .:? "completed_at"
              )

-- Clean a JSON value suitable for Postgresql's jsonb
clean :: Value -> Value
clean = cata (embed . f)
  where
    f (StringF a) = StringF $ T.filter (/= '\0000') a
    f x = x

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
