{- |
 - Module      : Data.Filesystem.FunFS.Ext2.LowLevel.BlockGroupDescriptor
 - Description : Block group descriptor.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Data.Filesystem.FunFS.Ext2.LowLevel.BlockGroupDescriptor ( BlockGroupDescriptor(..)
                                                                , BGDTable(..)
                                                                , sizeOfBGD
                                                                , emptyBGD
                                                                ) where
--
import Data.Filesystem.FunFS.Ext2.LowLevel.Struct
import Data.Word

-- Block-group descriptor.
struct "BlockGroupDescriptor"
       [ field "bgBlockBitmap"     [t|Word32|]    -- bg_block_bitmap (4 bytes)
       , field "bgInodeBitmap"     [t|Word32|]    -- bg_inode_bitmap (4 bytes)
       , field "bgInodeTable"      [t|Word32|]    -- bg_inode_table (4 bytes)
       , field "bgFreeBlocksCount" [t|Word16|]    -- bg_free_blocks_count (2 bytes)
       , field "bgFreeInodesCount" [t|Word16|]    -- bg_free_inodes_count (2 bytes)
       , field "bgUsedDirsCount"   [t|Word16|]    -- bg_used_dirs_count (2 bytes)
       , field "bgPad"             [t|Word16|]    -- bg_pad (2 bytes)
       , array "bgReserved"        [t|Word8|]  12 -- bg_reserved (12 bytes)
       ]
       ["Eq"]

newtype BGDTable = BGDTable { descriptors :: [BlockGroupDescriptor] }
                   deriving (Eq)

sizeOfBGD :: Integer
sizeOfBGD = sizeof emptyBGD

-- An empty BGD.
emptyBGD :: BlockGroupDescriptor
emptyBGD = BlockGroupDescriptor { bgBlockBitmap     = 0
                                , bgInodeBitmap     = 0
                                , bgInodeTable      = 0
                                , bgFreeBlocksCount = 0
                                , bgFreeInodesCount = 0
                                , bgUsedDirsCount   = 0
                                , bgPad             = 0
                                , bgReserved        = replicate 12 0
                                }

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show BGDTable where
    show (BGDTable descs) = concatMap g $ take (length descs) tuples
        where tuples  = iterate   f (0,head descs)
              f (i,_) = (i + 1,descs !! (i + 1))
              g (i,d) = "Block Group " ++ show i ++ ":\n" ++ show d

instance Show BlockGroupDescriptor where
    show bg = concat [ " `- Block bitmap : ", show $ bgBlockBitmap     bg, "\n"
                     , " `- Inode bitmap : ", show $ bgInodeBitmap     bg, "\n"
                     , " `- Inode table  : ", show $ bgInodeTable      bg, "\n"
                     , " `- Free blocks  : ", show $ bgFreeBlocksCount bg, "\n"
                     , " `- Free inodes  : ", show $ bgFreeInodesCount bg, "\n"
                     , " `- Directories  : ", show $ bgUsedDirsCount   bg, "\n"
                     ]
