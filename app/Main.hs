module Main where

import Control.Error

import Check.DiskSpaceUsage

main :: IO ()
main = do
  exceptT
    putStrLn
    (mapM_ print)
    freespace
