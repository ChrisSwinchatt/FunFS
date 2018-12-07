{-# LANGUAGE RecordWildCards #-}
module TestInode where
--
import Data.Bits
import Data.Char
import Data.FileSystem.FunFS.Ext2.LowLevel.Inode
import Data.FileSystem.FunFS.Util.Serialisable
import Data.FileSystem.FunFS.Util.SizeOf
import Data.Int
import Data.Word
import Numeric
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

inodeExpectedSize :: Int64
inodeExpectedSize = 128

instance Arbitrary Inode where
    arbitrary = do
        iMode           <- arbitrary
        iUid            <- arbitrary
        iSize           <- arbitrary
        iAtime          <- arbitrary
        iCtime          <- arbitrary
        iMtime          <- arbitrary
        iDtime          <- arbitrary
        iGid            <- arbitrary
        iLinksCount     <- arbitrary
        iBlocks         <- arbitrary
        iFlags          <- arbitrary
        iOsd1           <- arbitrary
        iDirect         <- vectorOf 12 arbitrary
        iSinglyIndirect <- arbitrary
        iDoublyIndirect <- arbitrary
        iTriplyIndirect <- arbitrary
        iGeneration     <- arbitrary
        iFileAcl        <- arbitrary
        iDirAcl         <- arbitrary
        iFaddr          <- arbitrary
        iOsd2           <- vectorOf 12 arbitrary
        return Inode {..}

prop_sizeOfInode :: Inode -> Bool
prop_sizeOfInode ino = sizeof ino == inodeExpectedSize

prop_encodeInode :: Inode -> Bool
prop_encodeInode ino = len == size && size == inodeExpectedSize
    where bs   = encode ino
          len  = B.length bs
          size = sizeof ino

prop_decodeInode :: Inode -> Bool
prop_decodeInode ino1 = ino1 == ino2
    where bs   = encode ino1
          ino2 = decode bs

testInode = do
    quickCheck prop_sizeOfInode
    quickCheck prop_encodeInode
    quickCheck prop_decodeInode
