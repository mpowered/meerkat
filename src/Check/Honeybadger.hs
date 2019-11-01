{-

neptune ~ % curl -u $AUTH_TOKEN: "https://app.honeybadger.io/v2/projects/55847/faults/summary?q=-is:resolved+-is:ignored+environment:production" | jq .
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   104    0   104    0     0     87      0 --:--:--  0:00:01 --:--:--    87
{
  "environments": [
    {
      "environment": "production",
      "resolved": false,
      "ignored": false,
      "count": 326
    }
  ],
  "total": 326
}

-}

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
        (  "q" =: ("-is:resolved+-is:ignored+environment:" <> env)
        <> basicAuth token ""
        )
  return [ HoneybadgerT { hbTime        = timestamp
                        , hbEnvironment = env
                        , hbFaults      = faultsTotal (responseBody faults)
                        }
         ]