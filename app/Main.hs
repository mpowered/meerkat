module Main where

import           Control.Error
import           Data.Text.Lazy        as Text
import           Data.Time.Clock.POSIX (getCurrentTime)
import           Network.HostName      (getHostName)

import           Check.DiskSpaceUsage

main :: IO ()
main = do
  timestamp <- getCurrentTime
  hostname  <- Text.pack <$> getHostName
  exceptT
    putStrLn
    (mapM_ print)
    (freespace hostname timestamp)
