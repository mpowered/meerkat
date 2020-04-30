{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.Mysql
  ( MysqlProcessList
  , MysqlProcessListT (..)
  , mysqlProcessList
  )
where

import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime)
import           Database
import           System.Process.Typed       as Proc
import           Xeno.DOM

mysql :: FilePath -> String -> ProcessConfig () () ()
mysql bin stmt = proc bin args
  where
    args = ["--batch", "-X", "-e", stmt]

parseProcessList :: UTCTime -> Text.Text -> LBS.ByteString -> Either String [MysqlProcessList]
parseProcessList time server doc =
  case parse (LBS.toStrict doc) of
    Left xerr -> Left $ show xerr
    Right node -> resultset time server node

resultset :: UTCTime -> Text.Text -> Node -> Either String [MysqlProcessList]
resultset time server node
  | name node == "resultset" = mapM (row time server) (children node)
  | otherwise = Left "expecting resultset"

row :: UTCTime -> Text.Text -> Node -> Either String MysqlProcessList
row time server node
  | name node == "row" = MysqlProcessListT <$> pure time
                                           <*> pure server
                                           <*> field node "Id"
                                           <*> field node "User"
                                           <*> field node "Host"
                                           <*> field node "Command"
                                           <*> field node "Time"
                                           <*> field node "State"
                                           <*> field node "Info"
                                           <*> field node "Progress"
  | otherwise = Left "expecting row"

class XmlVal a where
  fromXml :: Node -> Either String a

instance XmlVal a => XmlVal (Maybe a) where
  fromXml node
    | hasAttribute "xsi:nil" "true" node = Right Nothing
    | otherwise                          = Just <$> fromXml node

hasAttribute :: BS.ByteString -> BS.ByteString -> Node -> Bool
hasAttribute attr val node = (attr, val) `elem` attributes node

instance XmlVal Text.Text where
  fromXml = Right . Text.decodeUtf8 . nodeCatText

instance XmlVal Integer where
  fromXml = Right . read . C8.unpack . nodeCatText

instance XmlVal Double where
  fromXml = Right . read . C8.unpack . nodeCatText

nodeCatText :: Node -> BS.ByteString
nodeCatText = BS.concat . go . contents
  where
    go (Text x:xs) = x : go xs
    go (_:xs)      = go xs
    go []          = []

field :: XmlVal a => Node -> BS.ByteString -> Either String a
field node fname = getAlt (mconcat (map (Alt . go) $ children node) <|> Alt (Left "field not found"))
  where
    go child
      | name child == "field"
      , hasAttribute "name" fname child 
          = fromXml child
      | otherwise
          = Left "expecting field"

mysqlProcessList :: FilePath -> Text.Text -> UTCTime -> ExceptT String IO [MysqlProcessList]
mysqlProcessList bin server timestamp = do
  (stdout, _stderr) <- liftIO $ Proc.readProcess_ (mysql bin "show processlist")
  case parseProcessList timestamp server stdout of
    Left e -> throwE e
    Right node -> return node
