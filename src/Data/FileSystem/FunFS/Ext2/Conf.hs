{- |
 - Module      : Data.FileSystem.FunFS.Ext2.Conf
 - Description : Ext2 configuration.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.Conf where

import Data.FileSystem.FunFS.Ext2.LowLevel.SuperBlock
import Data.FileSystem.FunFS.Util.Bytes

{- |
 - Ext2 configuration. This can be used to:
 - 1. Create a superblock when making a new filesystem.
 - 2. Update a superblock when tuning an existing filesystem.
 - 3. Return a high-level 'extract' of the superblock when mounting a filesystem.
 -}
data Conf = Conf { volumeSize     :: Bytes  -- ^ The size of the volume.
                 , blockSize      :: Bytes  -- ^ The block size in bytes.
                 , blocksPerGroup :: Int    -- ^ The number of blocks per group.
                 , bytesPerInode  :: Bytes  -- ^ The number of bytes of disk space to allow per inode.
                 , maxMountCount  :: Int    -- ^ The maximum number of mounts before consistency check required.
                 , rBlocksCount   :: Int    -- ^ The number of reserved blocks.
                 , checkInterval  :: Int    -- ^ The interval in seconds between filesystem checks (0=never).
                 , volumeName     :: String -- ^ The volume label (16 characters).
                 }

minimumVolumeSize :: Bytes
minimumVolumeSize = MiB  1
defaultBlockSize :: Bytes
defaultBlockSize = KiB  4
defaultBytesPerInode :: Bytes
defaultBytesPerInode = KiB 16
defaultRBlocksCount :: Int
defaultRBlocksCount = 64
defaultCheckInterval :: Int
defaultCheckInterval = 2419200           -- 28 days
defaultVolumeName :: String
defaultVolumeName = "New Ext2 Volume"

-- | Default configuration.
defaultConf :: Conf
defaultConf = Conf { volumeSize     =              minimumVolumeSize
                   , blockSize      =              defaultBlockSize
                   , blocksPerGroup = fromIntegral defaultBlocksPerGroup
                   , bytesPerInode  =              defaultBytesPerInode
                   , maxMountCount  = fromIntegral defaultMaxMountCount
                   , rBlocksCount   =              defaultRBlocksCount
                   , checkInterval  =              defaultCheckInterval
                   , volumeName     =              defaultVolumeName
                   }
