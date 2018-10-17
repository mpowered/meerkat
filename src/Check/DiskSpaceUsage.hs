{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Check.DiskSpaceUsage
  ( DiskSpaceUsage (..)
  , freespace
  )
where

import           Control.Applicative        (empty)
import           Control.Error
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Char                  as Char
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as Text
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Void                  (Void)
import           System.Process.Typed       as Proc
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

data DiskSpaceUsage = DiskSpaceUsage
  { source      :: Text
  , fstype      :: Text
  , size        :: Integer
  , used        :: Integer
  , avail       :: Integer
  , target      :: Text
  } deriving Show

type Parser = M.Parsec Void Text

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

int :: Parser Integer
int = lexeme L.decimal

str :: Parser Text
str = lexeme $ M.takeWhileP (Just "string") (not . Char.isSpace)

usage :: Parser DiskSpaceUsage
usage = do
  source <- str
  fstype <- str
  size   <- int
  used   <- int
  avail  <- int
  target <- str
  return DiskSpaceUsage {..}

parser :: Parser [DiskSpaceUsage]
parser =
  M.manyTill C.anyChar C.eol *> M.many usage <* M.eof

df :: ProcessConfig () () ()
df = proc "df" args
  where
    args = ["-lk", "--output=source,fstype,size,used,avail,target"] ++ exclusions
    exclusions = concatMap exclude ["tmpfs", "devtmpfs"]
    exclude e = ["-x", e]

freespace :: ExceptT String IO [DiskSpaceUsage]
freespace = do
  (stdout, _stderr) <- liftIO $ Proc.readProcess_ df
  let txt = Text.decodeUtf8 stdout
  case M.parse parser "" txt of
    Left err -> throwE (M.parseErrorPretty' txt err)
    Right usages -> return usages
