{- |
 - Module      : Data.FileSystem.FunFS.Ext2.Volume
 - Description : Ext2 volume.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.Volume ( Volume
                                         , mountVolume
                                         , superBlock
                                         , blockList
                                         , bgdTable
                                         , getInode
                                         ) where

import Data.FileSystem.FunFS.Ext2.LowLevel.Block
import Data.FileSystem.FunFS.Ext2.LowLevel.Inode
import Data.FileSystem.FunFS.Ext2.LowLevel.BlockGroupDescriptor
import Data.FileSystem.FunFS.Ext2.LowLevel.SuperBlock
import Data.FileSystem.FunFS.Util.Strings
import Data.Int

import qualified Data.ByteString.Lazy as B

-- | An Ext2 volume.
data Volume = Volume { superBlock :: SuperBlock
                     , blockList  :: [Block]
                     , bgdTable   :: BGDTable
                     }
                     deriving (Eq)

volumeMinimumSize :: Int64
volumeMinimumSize = 2048

-- | Build Ext2 volume structure from a ByteString.
mountVolume :: B.ByteString -> Volume
mountVolume bs
    | B.length (B.take volumeMinimumSize bs) < volumeMinimumSize = error eDeviceTooSmall
    | otherwise = Volume { superBlock = sb
                        , blockList  = bl
                        , bgdTable   = readBGDTable sb bl
                        }
    where sb    = readSuperBlock bs
          n     = fromIntegral $ sBlocksCount sb
          bsize = fromIntegral $ getBlockSize sb
          bl    = take n $ makeBlockList bs bsize

-- Read the block group descriptor table.
readBGDTable :: SuperBlock -> [Block] -> BGDTable
readBGDTable sb blocks = BGDTable descs
   where   first   = fromIntegral $ sFirstDataBlock  sb + 1  -- First block of BGD table immediately follows superblock
           bsize   = fromIntegral $ getBlockSize     sb
           dcount  = fromIntegral $ countBlockGroups sb
           dsize   = fromIntegral sizeOfBGD
           bcount  = fromIntegral $ 1 + ((dcount*dsize) `div` bsize) -- Number of blocks containing the BGD table.
           blocks' = take bcount $ drop first blocks                 -- List of blocks containing the BGD table.
           dPerB   = bsize `div` dsize                               -- Number of descriptors per block.
           descs   = take dcount $ concatMap (\b -> take dPerB $ peekObjects b 0) blocks'

-- | Get an inode by number.
getInode :: Volume -> Int -> Inode
getInode vol inum
    | inum < 1 || inum >= count = error $ concat [eBadInode,": ",show inum]
    | otherwise                 = peekObject blk offset
    where sb     = superBlock   vol
          count  = fromIntegral $ sInodesCount sb
          blocks = blockList    vol
          bgdt   = bgdTable     vol
          descs  = descriptors  bgdt
          {- Compute the location of the inode entry on disk. Each block group has its own inode table so we first
           - calculate the block group and find the itable's block address. Then we compute which block of the table
           - contains the inode and the offset of the inode relative to that block, giving us the absolute block address
           - and block-relative offset at which the inode table entry resides.
           -
           - 1. Inputs:               let   ipg = 128               (inodes per group)
           -                              isize = 128               (inode size)
           -                              bsize = 4096              (block size)
           -                               inum = 64 - 1            (convert 1-based inode number to 0-based index)
           -}
          bsize = fromIntegral $ getBlockSize    sb :: Int
          isize = fromIntegral $ sInodeSize      sb :: Int
          ipg   = fromIntegral $ sInodesPerGroup sb :: Int
          inum' = inum - 1
          {-                                     inum     63
           - 2. Find the block group:       bg = ----   = --- = 0   (truncated)
           -                                     ipg      128
           -}
          bg = inum' `div` ipg
          {-
           - 3. Get inode table addr:   itable = bgInodeTable bg    (e.g. 10)
           -}
          itable = fromIntegral $ bgInodeTable $ descs !! bg
          {- 4. Get itable block index:          bsize    4096
           -    (a) Inodes per block:      ipb = -----  = ---- = 32
           -                                     isize   128
           -}
          ipb = bsize `div` isize
          {-                                     inum     63
           -    (b) Block index:          iblk = ----   = --- = 1   (truncated, relative to itable)
           -                                     ipb      32
           -}
          iblk = inum' `div` ipb
          {-    (c) Block address:        addr = itable + iblk
           -                                   =     10 + 1
           -                                   =     11
           -}
          addr = itable + iblk
          blk  = blocks !! addr
          {- 5. Get inode index:        index = inum % ipb
           -                                   =   63 % 32
           -                                   =   31               (inum is the 31st entry in iblk)
           -}
          index = inum' `mod` ipb
          {- 6. Get block offset:       offset = isize*index
           -                                   =   128*31
           -                                   =   3968             (inum begins at byte 3968 in iblk)
           -}
          offset = fromIntegral $ isize*index :: Int64
------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Volume where
    show vol = concat [ show $ superBlock vol
                      , "\n"
                      , show $ bgdTable   vol
                      ]
