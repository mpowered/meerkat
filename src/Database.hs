{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Text as Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)
import Database.Beam
  ( Beamable,
    C,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    Table (..),
    TableEntity,
    defaultDbSettings,
  )

data DB f = DB
  { dbDiskSpaceUsage :: f (TableEntity DiskSpaceUsageT),
    dbMemoryUsage :: f (TableEntity MemoryUsageT),
    dbProcessStats :: f (TableEntity ProcessStatsT),
    dbSidekiqQueues :: f (TableEntity SidekiqQueueT),
    dbSidekiqJobs :: f (TableEntity SidekiqJobT),
    dbBushpigJobs :: f (TableEntity BushpigJobT),
    dbPuma :: f (TableEntity PumaT),
    dbHoneybadger :: f (TableEntity HoneybadgerT),
    dbActionController :: f (TableEntity ActionControllerT),
    dbMysqlProcesslist :: f (TableEntity MysqlProcessListT)
  }
  deriving (Generic)

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings

data DiskSpaceUsageT f = DiskSpaceUsageT
  { duTime :: C f UTCTime,
    duHost :: C f Text,
    duSource :: C f Text,
    duFstype :: C f Text,
    duSize :: C f Integer,
    duUsed :: C f Integer,
    duAvail :: C f Integer,
    duTarget :: C f Text
  }
  deriving (Generic, Beamable)

instance Table DiskSpaceUsageT where
  data PrimaryKey DiskSpaceUsageT f = DiskSpaceUsageKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = DiskSpaceUsageKey <$> duTime <*> duHost <*> duTarget

type DiskSpaceUsage = DiskSpaceUsageT Identity

deriving instance Show DiskSpaceUsage

data ProcessStatsT f = ProcessStatsT
  { psTime :: C f UTCTime,
    psHost :: C f Text,
    psCommand :: C f Text,
    psCpu :: C f (Maybe Scientific),
    psUserCpu :: C f (Maybe Scientific),
    psSysCpu :: C f (Maybe Scientific),
    psGuestCpu :: C f (Maybe Scientific),
    psWaitCpu :: C f (Maybe Scientific),
    psVirtualMem :: C f (Maybe Integer),
    psResidentMem :: C f (Maybe Integer),
    psMem :: C f (Maybe Scientific)
  }
  deriving (Generic, Beamable)

instance Table ProcessStatsT where
  data PrimaryKey ProcessStatsT f = ProcessStatsKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = ProcessStatsKey <$> psTime <*> psHost <*> psCommand

type ProcessStats = ProcessStatsT Identity

deriving instance Show ProcessStats

data MemoryUsageT f = MemoryUsageT
  { muTime :: C f UTCTime,
    muHost :: C f Text,
    muTotalMem :: C f Integer,
    muUsedMem :: C f Integer,
    muFreeMem :: C f Integer,
    muSharedMem :: C f Integer,
    muBufferMem :: C f Integer,
    muCacheMem :: C f Integer,
    muAvailMem :: C f Integer,
    muTotalSwap :: C f Integer,
    muUsedSwap :: C f Integer,
    muFreeSwap :: C f Integer
  }
  deriving (Generic, Beamable)

instance Table MemoryUsageT where
  data PrimaryKey MemoryUsageT f = MemoryUsageKey (C f UTCTime) (C f Text) deriving (Generic, Beamable)
  primaryKey = MemoryUsageKey <$> muTime <*> muHost

type MemoryUsage = MemoryUsageT Identity

deriving instance Show MemoryUsage

data SidekiqQueueT f = SidekiqQueueT
  { sqTime :: C f UTCTime,
    sqQueue :: C f Text,
    sqClass :: C f Text,
    sqLength :: C f Integer,
    sqEnqueuedFor :: C f Double,
    sqJobIds :: C f (Vector Text)
  }
  deriving (Generic, Beamable)

instance Table SidekiqQueueT where
  data PrimaryKey SidekiqQueueT f = SidekiqQueueKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = SidekiqQueueKey <$> sqTime <*> sqQueue <*> sqClass

type SidekiqQueue = SidekiqQueueT Identity

deriving instance Show SidekiqQueue

data SidekiqJobT f = SidekiqJobT
  { sjJobId :: C f Text,
    sjQueue :: C f Text,
    sjClass :: C f Text,
    sjParams :: C f Value,
    sjEnqueuedAt :: C f UTCTime,
    sjStartedAt :: C f (Maybe UTCTime),
    sjCompletedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

instance Table SidekiqJobT where
  data PrimaryKey SidekiqJobT f = SidekiqJobKey (C f Text) deriving (Generic, Beamable)
  primaryKey = SidekiqJobKey <$> sjJobId

type SidekiqJob = SidekiqJobT Identity

deriving instance Show SidekiqJob

data BushpigJobT f = BushpigJobT
  { bjJobId :: C f Text,
    bjJobKey :: C f Text,
    bjHost :: C f Text,
    bjClass :: C f Text,
    bjParams :: C f Value,
    bjEnqueuedAt :: C f (Maybe UTCTime),
    bjStartedAt :: C f (Maybe UTCTime),
    bjCompletedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

instance Table BushpigJobT where
  data PrimaryKey BushpigJobT f = BushpigJobKey (C f Text) deriving (Generic, Beamable)
  primaryKey = BushpigJobKey <$> bjJobId

type BushpigJob = BushpigJobT Identity

deriving instance Show BushpigJob

data PumaT f = PumaT
  { pTime :: C f UTCTime,
    pHost :: C f Text,
    pWorker :: C f Integer,
    pBacklog :: C f Integer,
    pRunning :: C f Integer,
    pPoolCapacity :: C f Integer,
    pMaxThreads :: C f Integer
  }
  deriving (Generic, Beamable)

instance Table PumaT where
  data PrimaryKey PumaT f = PumaKey (C f UTCTime) (C f Text) (C f Integer) deriving (Generic, Beamable)
  primaryKey = PumaKey <$> pTime <*> pHost <*> pWorker

type Puma = PumaT Identity

deriving instance Show Puma

data HoneybadgerT f = HoneybadgerT
  { hbTime :: C f UTCTime,
    hbEnvironment :: C f Text,
    hbFaults :: C f Integer
  }
  deriving (Generic, Beamable)

instance Table HoneybadgerT where
  data PrimaryKey HoneybadgerT f = HoneybadgerKey (C f UTCTime) (C f Text) deriving (Generic, Beamable)
  primaryKey = HoneybadgerKey <$> hbTime <*> hbEnvironment

type Honeybadger = HoneybadgerT Identity

deriving instance Show Honeybadger

data ActionControllerT f = ActionControllerT
  { acTime :: C f UTCTime,
    acHost :: C f Text,
    acApp :: C f Text,
    acUserId :: C f (Maybe Text),
    acAccountId :: C f (Maybe Text),
    acScorecardId :: C f (Maybe Text),
    acController :: C f Text,
    acAction :: C f Text,
    acParams :: C f (Maybe Value),
    acFormat :: C f (Maybe Text),
    acMethod :: C f Text,
    acPath :: C f Text,
    acStatus :: C f (Maybe Text),
    acViewRuntime :: C f (Maybe Double),
    acDbRuntime :: C f (Maybe Double),
    acTotalRuntime :: C f (Maybe Double),
    acHeaders :: C f (Maybe Value)
  }
  deriving (Generic, Beamable)

instance Table ActionControllerT where
  data PrimaryKey ActionControllerT f = ActionControllerKey (C f UTCTime) (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = ActionControllerKey <$> acTime <*> acHost <*> acApp

type ActionController = ActionControllerT Identity

deriving instance Show ActionController

data MysqlProcessListT f = MysqlProcessListT
  { mplTime :: C f UTCTime,
    mplServer :: C f Text,
    mplId :: C f Integer,
    mplUser :: C f (Maybe Text),
    mplHost :: C f (Maybe Text),
    mplCommand :: C f (Maybe Text),
    mplPtime :: C f (Maybe Integer),
    mplState :: C f (Maybe Text),
    mplInfo :: C f (Maybe Text),
    mplProgress :: C f (Maybe Double)
  }
  deriving (Generic, Beamable)

instance Table MysqlProcessListT where
  data PrimaryKey MysqlProcessListT f = MysqlProcessListKey (C f UTCTime) (C f Text) (C f Integer) deriving (Generic, Beamable)
  primaryKey = MysqlProcessListKey <$> mplTime <*> mplServer <*> mplId

type MysqlProcessList = MysqlProcessListT Identity

deriving instance Show MysqlProcessList
