{- |
 - Module      : Data.FileSystem.FunFS.Ext2.LowLevel.Block
 - Description : Primitives for handling block-based I/O.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.LowLevel.Block ( Block
                                                 , bytes
                                                 , size
                                                 , makeBlock
                                                 , makeBlockList
                                                 , peekObject
                                                 , peekObjects
                                                 , pokeObject
                                                 , peek
                                                 , poke
                                                 ) where
--
import Data.FileSystem.FunFS.Util.Serialisable
import Data.FileSystem.FunFS.Util.SizeOf
import Data.FileSystem.FunFS.Util.Strings
import Data.Int
import Data.Word

import qualified Data.ByteString.Lazy as B

-- | A block of data.
data Block = Block { bytes  :: B.ByteString
                   , size   :: Int64
                   } deriving (Eq, Show)

-- | Make a Block from a ByteString.
makeBlock :: B.ByteString -> Int64 -> Block
makeBlock bs n
    | B.length sub < n = error eDeviceTooSmall
    | otherwise        = Block { bytes = sub, size = n }
    where sub = B.take n bs

-- | Divide a ByteString into a list of Blocks.
makeBlockList :: B.ByteString -> Int64 -> [Block]
makeBlockList bs s = map (`makeBlock` s) blocks
    where blocks = map fst $ tail $ iterate (\(_,r) -> B.splitAt s r) (B.empty,bs)

-- | Decode a Serialisable object starting at index 'i' in a block. The entire object must fit within the block.
peekObject :: (Serialisable a) => Block -> Int64 -> a
peekObject b i
    | i < 0       = error eNegativeIndex
    | i >= size b = error $ concat [eLargeIndex,": ",show i," >= ",show $ size b]
    | otherwise   = decode $ B.drop i $ bytes b

-- | Create an infinite list of Serialisable objects starting at index 'i' in a block.
peekObjects :: (Serialisable a, SizeOf a) => Block -> Int64 -> [a]
peekObjects b i = os
    where   o  = peekObject b i
            s  = sizeof o
            os = map fst $ iterate (\(_,i') -> (peekObject b i',i' + s)) (o,i + s)

-- | Encode a Serialisable object to index 'i' in a block. The entire object must fit within the block. The original
-- contents of the block (from i to the end of the object) are overwritten but the size of the block remains constant.
pokeObject :: (Serialisable a) => Block -> Int64 -> a -> Block
pokeObject b i o
    | i < 0       = error eNegativeIndex
    | i >= n1     = error eLargeIndex
    | i + n2 < n1 = b { bytes = bs3 }
    | otherwise   = error eBufferOverrun
    where   bs1 = bytes    b
            n1  = size     b
            bs2 = encode   o
            n2  = B.length bs2
            lhs = B.take   i        bs1
            rhs = B.append bs2      rhs
            bs3 = B.append lhs      rhs

-- | Extract a single byte from a block at a given index.
peek :: Block -> Int64 -> Word8
peek blk i
    | i < 0     = error eNegativeIndex
    | i >= n    = error eLargeIndex
    | otherwise = B.index bs i
    where bs = bytes blk
          n  = size  blk

-- | Insert a single byte into a block at a given index. The original byte is overwritten but the size of the block
-- remains constant.
poke :: Block -> Int64 -> Word8 -> Block
poke blk i w
    | i < 0        = error eNegativeIndex
    | i >= n       = error eLargeIndex
    | i == 0       = blk { bytes = B.cons w $ B.tail bs }
    | i == (n - 1) = blk { bytes = B.snoc (B.init bs) w }
    | otherwise    = blk { bytes = bs }
    where n  = size  blk
          bs = bytes blk
