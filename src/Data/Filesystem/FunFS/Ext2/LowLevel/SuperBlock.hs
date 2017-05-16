{- |
 - Module      : Data.Filesystem.FunFS.Ext2.LowLevel.SuperBlock
 - Description : Superblock
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
{-# LANGUAGE TemplateHaskell #-}
module Data.Filesystem.FunFS.Ext2.LowLevel.SuperBlock ( SuperBlock(..)
                                                      , State(..)
                                                      , Errors(..)
                                                      , superBlockMagic
                                                      , makeSuperBlock
                                                      , defaultSuperBlock
                                                      , checkSuperBlock
                                                      , readSuperBlock
                                                      , getBlockSize
                                                      , filesystemSize
                                                      , countBlockGroups
                                                      , defaultLogBlockSize
                                                      , defaultBlocksPerGroup
                                                      , defaultMaxMountCount
                                                      ) where

import Data.Bits
import Data.Char
import Data.Filesystem.FunFS.Ext2.LowLevel.Struct
import Data.Filesystem.FunFS.Util.Bytes
import Data.Filesystem.FunFS.Util.Time
import Data.Maybe
import Data.Word
import Numeric

import qualified Data.ByteString.Lazy            as B
import qualified Data.Filesystem.FunFS.Util.UUID as UUID

-- | Superblock.
struct "SuperBlock"
       [ field "sInodesCount"         [t|Word32|]    -- s_inodes_count (4 bytes)
       , field "sBlocksCount"         [t|Word32|]    -- s_blocks_count (4 bytes)
       , field "sRBlocksCount"        [t|Word32|]    -- s_r_blocks_count (4 bytes)
       , field "sFreeBlocksCount"     [t|Word32|]    -- s_free_blocks_count (4 bytes)
       , field "sFreeInodesCount"     [t|Word32|]    -- s_free_inodes_count (4 bytes)
       , field "sFirstDataBlock"      [t|Word32|]    -- s_first_data_block (4 bytes)
       , field "sLogBlockSize"        [t|Word32|]    -- s_log_block_size (4 bytes)
       , field "sLogFragSize"         [t|Word32|]    -- s_log_frag_size (4 bytes)
       , field "sBlocksPerGroup"      [t|Word32|]    -- s_blocks_per_group (4 bytes)
       , field "sFragsPerGroup"       [t|Word32|]    -- s_frags_per_group (4 bytes)
       , field "sInodesPerGroup"      [t|Word32|]    -- s_inodes_per_group (4 bytes)
       , field "sMtime"               [t|Word32|]    -- s_mtime (4 bytes)
       , field "sWtime"               [t|Word32|]    -- s_wtime (4 bytes)
       , field "sMountCount"          [t|Word16|]    -- s_mnt_count (2 bytes)
       , field "sMaxMountCount"       [t|Word16|]    -- s_max_mnt_count (2 bytes)
       , field "sMagic"               [t|Word16|]    -- s_magic (2 bytes)
       , field "sState"               [t|Word16|]    -- s_state (2 bytes)
       , field "sErrors"              [t|Word16|]    -- s_errors (2 bytes)
       , field "sMinorRevLevel"       [t|Word16|]    -- s_minor_rev_level (2 bytes)
       , field "sLastCheck"           [t|Word32|]    -- s_lastcheck (4 bytes)
       , field "sCheckInterval"       [t|Word32|]    -- s_checkinterval (4 bytes)
       , field "sCreatorOs"           [t|Word32|]    -- s_creator_os (4 bytes)
       , field "sRevLevel"            [t|Word32|]    -- s_rev_level (4 bytes)
       , field "sDefResUid"           [t|Word16|]    -- s_def_resuid (2 bytes)
       , field "sDefResGid"           [t|Word16|]    -- s_def_resgid (2 bytes)
       , field "sFirstIno"            [t|Word32|]    -- s_first_ino (4 bytes)
       , field "sInodeSize"           [t|Word16|]    -- s_inode_size (2 bytes)
       , field "sBlockGroupNr"        [t|Word16|]    -- s_block_group_nr (2 bytes)
       , field "sFeatureCompat"       [t|Word32|]    -- s_feature_compat (4 bytes)
       , field "sFeatureIncompat"     [t|Word32|]    -- s_feature_incompat (4 bytes)
       , field "sFeatureRoCompat"     [t|Word32|]    -- s_feature_ro_compat (4 bytes)
       , array "sUuid"                [t|Word8|]  16 -- s_uuid (16 bytes)
       , array "sVolumeName"          [t|Word8|]  16 -- s_volume_name (16 bytes)
       , array "sLastMounted"         [t|Word8|]  64 -- s_last_mounted (64 bytes)
       , field "sAlgoBitmap"          [t|Word32|]    -- s_algo_bitmap (4 bytes)
       , field "sPreallocBlocks"      [t|Word8|]     -- s_prealloc_blocks (1 bytes)
       , field "sPreallocDirBlocks"   [t|Word8|]     -- s_prealloc_dir_blocks (1 bytes)
       , field "alignment"            [t|Word16|]    -- alignment (2 bytes)
       , array "sJournalUuid"         [t|Word8|]  16 -- s_journal_uuid (16 bytes)
       , field "sJournalInum"         [t|Word32|]    -- s_journal_inum (4 bytes)
       , field "sJournalDev"          [t|Word32|]    -- s_journal_dev (4 bytes)
       , field "sLastOrphan"          [t|Word32|]    -- s_last_orphan (4 bytes)
       , array "sHashSeed"            [t|Word32|]  4 -- s_hash_seed (4x4 bytes)
       , field "sDefHashVersion"      [t|Word8|]     -- s_def_hash_version (1 bytes)
       , array "padding"              [t|Word8|]   3 -- padding (3 bytes)
       , field "sDefaultMountOptions" [t|Word32|]    -- s_default_mount_options (4 bytes)
       , field "sFirstMetaBg"         [t|Word32|]    -- s_first_meta_bg (4 bytes)
       ]
       ["Eq"]

-- | Filesystem state.
data State = ValidFS -- ^ Filesystem cleanly unmounted.
           | ErrorFS -- ^ Filesystem may contain errors.
           deriving (Eq, Ord)

-- | Value indicating what the
data Errors = Continue -- ^ Continue as if nothing happened.
            | RO       -- ^ Remount as read-only.
            | Panic    -- ^ Trigger kernel panic.
            deriving (Eq, Ord)

-- | The superblock magic number.
superBlockMagic :: Word16
superBlockMagic = 0xEF53

-- | Check whether the superblock is valid. If it is, the superblock is returned. If not, an error string describing the
-- first problem detected is returned.
checkSuperBlock :: SuperBlock -> SuperBlock
checkSuperBlock sb
    | sMagic           sb /= superBlockMagic                  = error eBadSuperBlock
    -- | sRevLevel        sb >  defaultRevLevel                  = error eBadVersion
    | sInodesPerGroup  sb >  fromIntegral (8*getBlockSize sb) = error eBadSuperBlock
    | sState           sb == fromIntegral (fromEnum ErrorFS)  = error eNeedFsck
    -- | sFeatureIncompat sb >  defaultFeatureIncompat           = error eFeatureIncompat
    | otherwise                                               = sb

-- | Try to decode a superblock from a bytestring.
--
-- NB: The superblock always starts at the 1024th byte and is 1024 bytes long. Therefore the bytestring must be
-- at least 2048 bytes long. This is not checked for performance reasons.
--
-- If the superblock magic number is correct, then the superblock is returned.
-- If the magic number is incorrect, the superblock is either not valid or has been corrupted, and Nothing is returned.
-- In this case, the filesystem may be recoverable using fsck(8).
readSuperBlock :: B.ByteString -> SuperBlock
readSuperBlock = checkSuperBlock . decode . B.drop 1024

-- | Get the block size of a filesystem. This requires extra computation from the sLogBlockSize field of the superblock.
getBlockSize :: SuperBlock -> Int
getBlockSize sb = 1024 `shiftL` fromIntegral (sLogBlockSize sb)

-- | Compute the size of the filesystem.
filesystemSize :: SuperBlock -> Int
filesystemSize sb = count * size
    where count = fromIntegral $ sBlocksCount sb
          size  = getBlockSize sb

-- | Compute the number of block groups in the filesystem.
countBlockGroups :: SuperBlock -> Int
countBlockGroups sb
    | count < 1 = 1
    | otherwise = count
    where blocks = fromIntegral $ sBlocksCount    sb
          bpg    = fromIntegral $ sBlocksPerGroup sb
          count  = blocks `div` bpg

makeSuperBlock :: String -> Word32 -> Word32 -> Word32 -> Word32 -> Word16 -> Word32 -> Word32 -> IO SuperBlock
makeSuperBlock name size blksz bpg bpi mmc rbc ci = do
    uuid <- UUID.nextRandom
    let sb = defaultSuperBlock { sInodesCount     = ic
                               , sBlocksCount     = bc
                               , sRBlocksCount    = rbc
                               , sLogBlockSize    = log2 blksz - 10
                               , sBlocksPerGroup  = bpg
                               , sFragsPerGroup   = bpg
                               , sFreeInodesCount = ic - sFirstIno       sb
                               , sFreeBlocksCount = bc - sFirstDataBlock sb
                               , sMtime           = 0
                               , sWtime           = 0
                               , sLastCheck       = 0
                               , sCheckInterval   = ci
                               , sUuid            = UUID.toBytes uuid
                               , sVolumeName      = take 16 $ map (fromIntegral . ord) name
                               , sMaxMountCount   = mmc
                               }
    return $ checkSuperBlock sb
    where ic   = size `div` bpi
          bc   = size `div` blksz
          log2 x
            | x <= 0    = undefined
            | x == 1    = 0
            | otherwise = 1 + log2 (x `div` 2)

{- |
 - A basic superblock with sensible default values for some members.
 -
 - Can be used as the basis for a new superblock when creating a filesystem.
 -
 - NB: The following members are undefined and must be updated:
 -     sInodesCount
 -     sBlocksCount
 -     sRBlocksCount
 -     sFreeBlocksCount
 -     sFreeInodesCount
 -     sMtime
 -     sWtime
 -     sLastCheck
 -     sCheckInterval
 -     sUuid
 -     sVolumeName
 - NB: sFirstDataBlock should be updated if the block size is changed. Its value is 0 if the block size is greater than
 - 1024. If the block size is 1024, then sFirstDataBlock must have the value 1.
 -}
defaultSuperBlock :: SuperBlock
defaultSuperBlock = SuperBlock { sInodesCount         = undefined
                               , sBlocksCount         = undefined
                               , sRBlocksCount        = undefined
                               , sFreeBlocksCount     = undefined
                               , sFreeInodesCount     = undefined
                               , sFirstDataBlock      = defaultFirstDataBlock
                               , sLogBlockSize        = defaultLogBlockSize
                               , sLogFragSize         = defaultLogFragSize
                               , sBlocksPerGroup      = defaultBlocksPerGroup
                               , sFragsPerGroup       = defaultFragsPerGroup
                               , sInodesPerGroup      = defaultInodesPerGroup
                               , sMtime               = undefined
                               , sWtime               = undefined
                               , sMountCount          = 0
                               , sMaxMountCount       = defaultMaxMountCount
                               , sMagic               = superBlockMagic
                               , sState               = defaultState
                               , sErrors              = defaultErrors
                               , sMinorRevLevel       = defaultMinorRevLevel
                               , sLastCheck           = undefined
                               , sCheckInterval       = undefined
                               , sCreatorOs           = 0
                               , sRevLevel            = defaultRevLevel
                               , sDefResUid           = defaultResUid
                               , sDefResGid           = defaultResGid
                               , sFirstIno            = defaultFirstIno
                               , sInodeSize           = defaultInodeSize
                               , sBlockGroupNr        = defaultBlockGroupNr
                               , sFeatureCompat       = defaultFeatureCompat
                               , sFeatureIncompat     = defaultFeatureIncompat
                               , sFeatureRoCompat     = defaultFeatureRoCompat
                               , sUuid                = undefined
                               , sVolumeName          = undefined
                               , sLastMounted         = [0]
                               , sAlgoBitmap          = 0
                               , sPreallocBlocks      = 0
                               , sPreallocDirBlocks   = 0
                               , alignment            = 0
                               , sJournalUuid         = replicate 16 0
                               , sJournalInum         = 0
                               , sJournalDev          = 0
                               , sLastOrphan          = 0
                               , sHashSeed            = replicate 4 0
                               , sDefHashVersion      = 0
                               , padding              = replicate 3 0
                               , sDefaultMountOptions = 0
                               , sFirstMetaBg         = 0
                               }

-- CONSTANTS

-- If the block size is greater than 1024, the first data block is always 0. Otherwise it is always 1.
defaultFirstDataBlock :: Word32
defaultFirstDataBlock = if defaultLogBlockSize == 0 then 1 else 0

-- sLogBlockSize = log(2,blockSize) - 10
--     blockSize = 1024 << sLogBlockSize
defaultLogBlockSize :: Word32
defaultLogBlockSize = 2

-- Fragments are not implemented in Ext2 so we just copy the block size.
-- This is equivalent to saying each block has exactly one fragment.
defaultLogFragSize :: Word32
defaultLogFragSize = defaultLogBlockSize

-- Chosen arbitrarily.
defaultBlocksPerGroup :: Word32
defaultBlocksPerGroup = 32768

-- As fragments are not implemented, there should always be the same number of fragments as blocks.
defaultFragsPerGroup :: Word32
defaultFragsPerGroup = defaultBlocksPerGroup

-- The maximum number of inodes in a block group is 8*blockSize.
defaultInodesPerGroup :: Word32
defaultInodesPerGroup = blockBits
    where blockSize = 1024 `shiftL` (fromIntegral defaultLogBlockSize :: Int)
          blockBits = (   8 :: Word32) * blockSize

-- Chosen arbitrarily
defaultMaxMountCount :: Word16
defaultMaxMountCount = 20

-- Default filesystem state.
defaultState :: Word16
defaultState = fromIntegral $ fromEnum ValidFS

-- Default error handling action.
defaultErrors :: Word16
defaultErrors = fromIntegral $ fromEnum RO

-- Chosen arbitrarily
defaultMinorRevLevel :: Word16
defaultMinorRevLevel = 0

-- Revision 0
defaultRevLevel :: Word32
defaultRevLevel = 0

-- UID of reserved blocks defaults to 0
defaultResUid :: Word16
defaultResUid = 0

-- GID of reserved blocks defaults to 0
defaultResGid :: Word16
defaultResGid = 0

-- In revision 0, the first non-reserved inode is always 11.
defaultFirstIno :: Word32
defaultFirstIno = 11

-- In revision 0, the inode size is always 128
defaultInodeSize :: Word16
defaultInodeSize = 128

-- Block group number of the superblock. This is always the first data block for the main superblock but varies for
-- backup blocks. In revision 0, every block group has a backup of the superblock.
defaultBlockGroupNr :: Word16
defaultBlockGroupNr = fromIntegral defaultFirstDataBlock

-- Ignore if unsupported feature flags.
defaultFeatureCompat :: Word32
defaultFeatureCompat = 0

-- Refuse to mount if unsupported feature flags.
defaultFeatureIncompat :: Word32
defaultFeatureIncompat = 0

-- Mount as read-only if unsupported feature flags.
defaultFeatureRoCompat :: Word32
defaultFeatureRoCompat = 0

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Enum State where
    fromEnum ValidFS = 1
    fromEnum ErrorFS = 2
    toEnum   1       = ValidFS
    toEnum   2       = ErrorFS
    toEnum   x       = error $ show x

instance Show State where
    show ValidFS = "Clean"
    show ErrorFS = "Unclean"

instance Enum Errors where
   fromEnum Continue = 1
   fromEnum RO       = 2
   fromEnum Panic    = 3
   toEnum   1       = Continue
   toEnum   2       = RO
   toEnum   3       = Panic
   toEnum   x       = error $ show x

instance Show Errors where
    show Continue = "Continue"
    show RO       = "Remount read-only"
    show Panic    = "Panic"

getSuperBlockFields :: SuperBlock -> [(String,String)]
getSuperBlockFields sb = [ ("Filesystem label",         label)
                         , ("Filesystem UUID",          uuid)
                         , ("Filesystem size",          size)
                         , ("Filesystem version",       version)
                         , ("Filesystem magic number",  magic)
                         , ("Filesystem state",         state)
                         , ("Error handling behaviour", errors)
                         , ("Filesystem OS type",       ostype)
                         , ("Total inodes",             icount)
                         , ("Free inodes",              ficount)
                         , ("First free inode",         freeino)
                         , ("Inode size",               inosize)
                         , ("Total blocks",             bcount)
                         , ("Reserved blocks",          rbcount)
                         , ("Free blocks",              fbcount)
                         , ("First data block",         fbaddr)
                         , ("Block size",               blksize)
                         , ("Blocks per group",         bsPerG)
                         , ("Inodes per group",         isPerG)
                         , ("Last mount time",          mtime)
                         , ("Last write time",          wtime)
                         , ("Mount count",              mcount)
                         , ("Maximum mount count",      mmcount)
                         , ("Last checked",             checked)
                         , ("Check interval",           interval)
                         , ("UID of reserved blocks",   resUID)
                         , ("GID of reserved blocks",   resGID)
                         ]
    where   label    = flip arrayToString 16                                     $  sVolumeName     sb
            uuid     = (UUID.toString . fromJust . UUID.fromByteString . B.pack) $ sUuid            sb
            size     = show $ byteCount                                          $ filesystemSize   sb
            version  = show                                                      $ sRevLevel        sb
            magic    = "0x" ++ map toUpper                                       ( showHex  (sMagic sb) "")
            state    = show (toEnum (fromIntegral                                $ sState           sb) :: State)
            errors   = show (toEnum (fromIntegral                                $ sErrors          sb) :: Errors)
            ostype   = show                                                      $ sCreatorOs       sb
            icount   = show                                                      $ sInodesCount     sb
            ficount  = show                                                      $ sFreeInodesCount sb
            freeino  = show                                                      $ sFirstIno        sb
            inosize  = show                                                      $ sInodeSize       sb
            bcount   = show                                                      $ sBlocksCount     sb
            rbcount  = show                                                      $ sRBlocksCount    sb
            fbcount  = show                                                      $ sFreeBlocksCount sb
            fbaddr   = show                                                      $ sFirstDataBlock  sb
            blksize  = show                                                      $ getBlockSize     sb
            bsPerG   = show                                                      $ sBlocksPerGroup  sb
            isPerG   = show                                                      $ sInodesPerGroup  sb
            mtime    = showTimeStamp                                             $ sMtime           sb
            wtime    = showTimeStamp                                             $ sWtime           sb
            mcount   = show                                                      $ sMountCount      sb
            mmcount  = show                                                      $ sMaxMountCount   sb
            checked  = showTimeStamp                                             $ sLastCheck       sb
            interval = show                                                      $ sCheckInterval   sb
            resUID   = show                                                      $ sDefResUid       sb
            resGID   = show                                                      $ sDefResGid       sb

instance Show SuperBlock where
    show sb = "Superblock:\n" ++ concatMap f fields
        where fields  = getSuperBlockFields sb
              width   = maximum $ map (length . fst) fields
              f (l,v) = let pad = width - length l
                        in  concat [" `- ",l,replicate pad ' '," : ",v,"\n"]
