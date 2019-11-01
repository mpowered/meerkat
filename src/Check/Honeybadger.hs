{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Check.Honeybadger
  ( Honeybadger
  , HoneybadgerT (..)
  , honeybadgerFaults
  )
where

import           Control.Error
import           Data.ByteString            (ByteString)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (UTCTime)
import           Data.Yaml
import           Database
import           Network.HTTP.Req

data Faults = Faults
  { faultsTotal       :: Integer
  }

instance FromJSON Faults where
  parseJSON = withObject "Faults" $ \o ->
    Faults
      <$> o .: "total"

honeybadgerFaults :: Integer -> Text.Text -> ByteString -> UTCTime -> ExceptT String IO [Honeybadger]
honeybadgerFaults projectId env token timestamp = do
  faults <- runReq defaultHttpConfig $
    req GET
        (https "app.honeybadger.io" /: "v2" /: "projects" /~ projectId /: "faults" /: "summary")
        NoReqBody
        jsonResponse
        (  "q" =: ("-is:resolved -is:ignored environment:" <> env)
        <> basicAuth token ""
        )
  return [ HoneybadgerT { hbTime        = timestamp
                        , hbEnvironment = env
                        , hbFaults      = faultsTotal (responseBody faults)
                        }
         ]