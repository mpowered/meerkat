{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.ProcessStatistics
  ( ProcessStats
  , ProcessStatsT (..)
  , processes
  )
where

import           Control.Applicative        (empty)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Char                  as Char
import           Data.Scientific
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Time.Clock            (UTCTime (..), getCurrentTime)
import           Data.Void                  (Void)
import           Database
import           System.Process.Typed       as Proc
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

{-
# Time        UID       PID    %usr %system  %guest   %wait    %CPU   CPU  minflt/s  majflt/s     VSZ     RSS   %MEM   kB_rd/s   kB_wr/s kB_ccwr/s iodelay  Command
02:10:09 PM     0       488    0.00    0.00    0.00    0.00    0.00     3    203.00      0.00  135124    6384   0.04      0.00      0.00      0.00       0  pidstat
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

sci :: Parser Scientific
sci = lexeme (L.signed sc L.scientific)

str :: Parser Text.Text
str = LText.toStrict <$> lexeme (M.takeWhileP (Just "string") (not . Char.isSpace))

header :: Parser [Text.Text]
header = symbol "#" *> M.many str

pidstats :: [Text.Text] -> UTCTime -> Text.Text -> Parser ProcessStats
pidstats fields timestamp host = do
  stats <- foldM field blank fields
  when (psTime stats == psTime blank) $
    fail "Unable to find time when parsing process statistics"
  when (psCommand stats == psCommand blank) $
    fail "Unable to find command when parsing process statistics"
  return stats

  where
    -- field ps "Time"     = (\x -> ps { psTime = x }) <$> (parseTimeM False defaultTimeLocale "fmt" =<< Text.unpack <$> str)
    field ps "Command"  = (\x -> ps { psCommand = x }) <$> str
    field ps "%CPU"     = (\x -> ps { psCpu = Just x }) <$> sci
    field ps "%usr"     = (\x -> ps { psUserCpu = Just x }) <$> sci
    field ps "%system"  = (\x -> ps { psSysCpu = Just x }) <$> sci
    field ps "%guest"   = (\x -> ps { psGuestCpu = Just x }) <$> sci
    field ps "%wait"    = (\x -> ps { psWaitCpu = Just x }) <$> sci
    field ps "VSZ"      = (\x -> ps { psVirtualMem = Just x }) <$> int
    field ps "RSS"      = (\x -> ps { psResidentMem = Just x }) <$> int
    field ps "%MEM"     = (\x -> ps { psMem = Just x }) <$> sci
    field ps _          = return ps

    blank =
      ProcessStatsT
        { psTime        = timestamp
        , psHost        = host
        , psCommand     = ""
        , psCpu         = Nothing
        , psUserCpu     = Nothing
        , psSysCpu      = Nothing
        , psGuestCpu    = Nothing
        , psWaitCpu     = Nothing
        , psVirtualMem  = Nothing
        , psResidentMem = Nothing
        , psMem         = Nothing
        }

parser :: Text.Text -> UTCTime -> Parser [ProcessStats]
parser host timestamp = go []
  where
    go [] = do
      line <- M.eitherP header (M.skipMany str) <* C.eol
      case line of
        Left headers' -> go headers'
        Right () -> go []
    go headers = do
      line <- M.eitherP header (pidstats headers timestamp host) <* C.eol
      case line of
        Left headers' -> go headers'
        Right stats -> do
          stats' <- go headers
          return (stats : stats')

pidstat :: ProcessConfig () () ()
pidstat = proc "pidstat" ["60", "1", "-druh"]

processes :: Text.Text -> ExceptT String IO [ProcessStats]
processes host = do
  (stdout, _stderr) <- liftIO $ Proc.readProcess_ pidstat
  -- Only the time (not date) is able to be parsed, for now we override times
  -- with the current time _after_ the process has completed.
  now <- liftIO getCurrentTime
  let txt = Text.decodeUtf8 stdout
  case M.parse (parser host now) "" txt of
    Left errmsg  -> throwE (M.parseErrorPretty' txt errmsg)
    Right usages -> return usages
