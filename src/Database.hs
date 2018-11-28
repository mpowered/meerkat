{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Database where

import           Data.Scientific
import           Data.Text                as Text
import           Data.Time.Clock
import           Database.Beam

data DB f = DB
  { dbDiskSpaceUsage :: f (TableEntity DiskSpaceUsageT)
  , dbMemoryUsage    :: f (TableEntity MemoryUsageT)
  , dbProcessStats   :: f (TableEntity ProcessStatsT)
  , dbSidekiqQueues  :: f (TableEntity SidekiqQueueT)
  } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

data DiskSpaceUsageT f = DiskSpaceUsageT
  { duTime        :: C f UTCTime
  , duHost        :: C f Text
  , duSource      :: C f Text
  , duFstype      :: C f Text
  , duSize        :: C f Integer
  , duUsed        :: C f Integer
  , duAvail       :: C f Integer
  , duTarget      :: C f Text
  } deriving (Generic, Beamable)

instance Table DiskSpaceUsageT where
  data PrimaryKey DiskSpaceUsageT f = DiskSpaceUsageKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = DiskSpaceUsageKey <$> duTime <*> duHost <*> duTarget

type DiskSpaceUsage = DiskSpaceUsageT Identity
deriving instance Show DiskSpaceUsage

data ProcessStatsT f = ProcessStatsT
  { psTime        :: C f UTCTime
  , psHost        :: C f Text
  , psCommand     :: C f Text
  , psCpu         :: C f (Maybe Scientific)
  , psUserCpu     :: C f (Maybe Scientific)
  , psSysCpu      :: C f (Maybe Scientific)
  , psGuestCpu    :: C f (Maybe Scientific)
  , psWaitCpu     :: C f (Maybe Scientific)
  , psVirtualMem  :: C f (Maybe Integer)
  , psResidentMem :: C f (Maybe Integer)
  , psMem         :: C f (Maybe Scientific)
  } deriving (Generic, Beamable)

instance Table ProcessStatsT where
  data PrimaryKey ProcessStatsT f = ProcessStatsKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = ProcessStatsKey <$> psTime <*> psHost <*> psCommand

type ProcessStats = ProcessStatsT Identity
deriving instance Show ProcessStats

data MemoryUsageT f = MemoryUsageT
  { muTime        :: C f UTCTime
  , muHost        :: C f Text
  , muTotalMem    :: C f Integer
  , muUsedMem     :: C f Integer
  , muFreeMem     :: C f Integer
  , muSharedMem   :: C f Integer
  , muBufferMem   :: C f Integer
  , muCacheMem    :: C f Integer
  , muAvailMem    :: C f Integer
  , muTotalSwap   :: C f Integer
  , muUsedSwap    :: C f Integer
  , muFreeSwap    :: C f Integer
  } deriving (Generic, Beamable)

instance Table MemoryUsageT where
  data PrimaryKey MemoryUsageT f = MemoryUsageKey (C f UTCTime) (C f Text) deriving (Generic, Beamable)
  primaryKey = MemoryUsageKey <$> muTime <*> muHost

type MemoryUsage = MemoryUsageT Identity
deriving instance Show MemoryUsage

data SidekiqQueueT f = SidekiqQueueT
  { sqTime        :: C f UTCTime
  , sqQueue       :: C f Text
  , sqLength      :: C f Integer
  } deriving (Generic, Beamable)

instance Table SidekiqQueueT where
  data PrimaryKey SidekiqQueueT f = SidekiqQueueKey (C f UTCTime) (C f Text) deriving (Generic, Beamable)
  primaryKey = SidekiqQueueKey <$> sqTime <*> sqQueue

type SidekiqQueue = SidekiqQueueT Identity
deriving instance Show SidekiqQueue
