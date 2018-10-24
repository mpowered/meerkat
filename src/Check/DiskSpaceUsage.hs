{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.DiskSpaceUsage
  ( DiskSpaceUsage
  , DiskSpaceUsageT (..)
  , freespace
  )
where

import           Control.Applicative        (empty)
import           Control.Error
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Char                  as Char
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Time.Clock            (UTCTime)
import           Data.Void                  (Void)
import           Database
import           System.Process.Typed       as Proc
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void LText.Text

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

int :: Parser Integer
int = lexeme L.decimal

str :: Parser Text.Text
str = LText.toStrict <$> lexeme (M.takeWhileP (Just "string") (not . Char.isSpace))

usage :: Text.Text -> UTCTime -> Parser DiskSpaceUsage
usage duHost duTime = do
  duSource <- str
  duFstype <- str
  duSize   <- int
  duUsed   <- int
  duAvail  <- int
  duTarget <- str
  return DiskSpaceUsageT {..}

parser :: Text.Text -> UTCTime -> Parser [DiskSpaceUsage]
parser host time =
  M.manyTill C.anyChar C.eol *> M.many (usage host time) <* M.eof

df :: ProcessConfig () () ()
df = proc "df" args
  where
    args = ["-lk", "--output=source,fstype,size,used,avail,target"] ++ exclusions
    exclusions = concatMap exclude ["tmpfs", "devtmpfs"]
    exclude e = ["-x", e]

freespace :: Text.Text -> UTCTime -> ExceptT String IO [DiskSpaceUsage]
freespace host timestamp = do
  (stdout, _stderr) <- liftIO $ Proc.readProcess_ df
  let txt = Text.decodeUtf8 stdout
  case M.parse (parser host timestamp) "" txt of
    Left errmsg  -> throwE (M.parseErrorPretty' txt errmsg)
    Right usages -> return usages
