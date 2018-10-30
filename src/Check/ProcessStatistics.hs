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

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Char                  as Char
import           Data.Scientific
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Time.Clock            (UTCTime (..), getCurrentTime)
import           Data.Time.Format           (parseTimeM, defaultTimeLocale)
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
sc = L.space space1 empty empty

space1 :: Parser ()
space1 = M.skipSome (M.label "whitespace" $ C.oneOf [' ', '\t'])

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: LText.Text -> Parser LText.Text
symbol = L.symbol sc

int :: Parser Integer
int = lexeme L.decimal

sci :: Parser Scientific
sci = lexeme (L.signed sc L.scientific)

str :: Parser Text.Text
str = LText.toStrict <$> lexeme (M.takeWhile1P (Just "string") (not . Char.isSpace))

utctime :: Parser UTCTime
utctime = do
  timestr <- Text.unwords <$> M.count 2 str
  parseTimeM False defaultTimeLocale "%r" (Text.unpack timestr)

header :: Parser [Text.Text]
header = symbol "#" *> M.many str

pidstats :: [Text.Text] -> Text.Text -> UTCTime -> Parser ProcessStats
pidstats fields host timestamp = do
  stats <- foldM field blank fields
  when (psCommand stats == psCommand blank) $
    fail "Unable to find command when parsing process statistics"
  return stats

  where
    field ps "Time"     = ps <$ utctime
    field ps "Command"  = (\x -> ps { psCommand = Text.unwords x }) <$> many str
    field ps "%CPU"     = (\x -> ps { psCpu = Just x }) <$> sci
    field ps "%usr"     = (\x -> ps { psUserCpu = Just x }) <$> sci
    field ps "%system"  = (\x -> ps { psSysCpu = Just x }) <$> sci
    field ps "%guest"   = (\x -> ps { psGuestCpu = Just x }) <$> sci
    field ps "%wait"    = (\x -> ps { psWaitCpu = Just x }) <$> sci
    field ps "VSZ"      = (\x -> ps { psVirtualMem = Just x }) <$> int
    field ps "RSS"      = (\x -> ps { psResidentMem = Just x }) <$> int
    field ps "%MEM"     = (\x -> ps { psMem = Just x }) <$> sci
    field ps _          = ps <$ str

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

data Line
  = Header [Text.Text]
  | Stats ProcessStats
  | Preamble
  | EOF

line :: Text.Text -> UTCTime -> [Text.Text] -> Parser Line
line host timestamp headers =
      (EOF <$ M.eof)
  <|> (Header <$> header <* C.eol)
  <|> (Stats <$> pidstats headers host timestamp <* C.eol)

preamble :: Parser Line
preamble = (EOF <$ M.eof)
       <|> (Header <$> header <* C.eol)
       <|> (Preamble <$ M.label "preamble" (M.many str) <* C.eol)

parser :: Text.Text -> UTCTime -> Parser [ProcessStats]
parser host timestamp = go []
  where
    go headers = do
      l <- if null headers then preamble else line host timestamp headers
      case l of
        EOF -> return []
        Header h -> go h
        Stats ps -> do
          ps' <- go headers
          return (ps : ps')
        Preamble -> go headers

pidstat :: ProcessConfig () () ()
pidstat = proc "pidstat" ["60", "1", "-druhl"]

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
