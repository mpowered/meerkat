{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Check.MemoryUsage
  ( MemoryUsage,
    MemoryUsageT (..),
    memoryUsage,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Error (ExceptT, throwE)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as Text
import Data.Time.Clock (UTCTime)
import Data.Void (Void)
import Database (MemoryUsage, MemoryUsageT (..))
import System.Process.Typed as Proc
  ( ProcessConfig,
    proc,
    readProcess_,
  )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-
              total        used        free      shared     buffers       cache   available
Mem:    34283200512  5818839040 28229505024    18145280    34848768   200007680 28327419904
Swap:    4294967296           0  4294967296
-}

type Parser = M.Parsec Void LText.Text

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: LText.Text -> Parser LText.Text
symbol = L.symbol sc

int :: Parser Integer
int = lexeme L.decimal

header :: Parser ()
header = do
  C.space1
  void $ symbol "total"
  void $ symbol "used"
  void $ symbol "free"
  void $ symbol "shared"
  void $ symbol "buffers"
  void $ symbol "cache"
  void $ symbol "available"

vals :: Text.Text -> UTCTime -> Parser MemoryUsage
vals host timestamp =
  MemoryUsageT
    <$> pure timestamp
    <*> pure host
    <* symbol "Mem:"
    <*> int
    <*> int
    <*> int
    <*> int
    <*> int
    <*> int
    <*> int
    <* symbol "Swap:"
    <*> int
    <*> int
    <*> int

parser :: Text.Text -> UTCTime -> Parser MemoryUsage
parser host timestamp = do
  header
  vals host timestamp

free :: FilePath -> ProcessConfig () () ()
free bin = proc bin ["-bw"]

memoryUsage :: FilePath -> Text.Text -> UTCTime -> ExceptT String IO [MemoryUsage]
memoryUsage bin host timestamp = do
  (stdout, _stderr) <- liftIO $ Proc.readProcess_ (free bin)
  let txt = Text.decodeUtf8 stdout
  case M.parse (parser host timestamp) "" txt of
    Left errbundle -> throwE (M.errorBundlePretty errbundle)
    Right usage -> return [usage]
