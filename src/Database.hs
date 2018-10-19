{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Database where

import           Data.Text.Lazy           as Text
import           Data.Time.Clock
import           Database.Beam

data DB f = DB
  { diskSpaceUsage :: f (TableEntity DiskSpaceUsageT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

data DiskSpaceUsageT f = DiskSpaceUsageT
  { timestamp   :: C f UTCTime
  , host        :: C f Text
  , source      :: C f Text
  , fstype      :: C f Text
  , size        :: C f Integer
  , used        :: C f Integer
  , avail       :: C f Integer
  , target      :: C f Text
  } deriving (Generic, Beamable)

instance Table DiskSpaceUsageT where
  data PrimaryKey DiskSpaceUsageT f = DiskSpaceUsageKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = DiskSpaceUsageKey <$> timestamp <*> host <*> target

type DiskSpaceUsage = DiskSpaceUsageT Identity
deriving instance Show DiskSpaceUsage
