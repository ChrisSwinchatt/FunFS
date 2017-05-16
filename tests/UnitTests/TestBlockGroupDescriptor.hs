{-# LANGUAGE RecordWildCards #-}
module TestBlockGroupDescriptor where
--
import Data.Filesystem.FunFS.Ext2.LowLevel.BlockGroupDescriptor
import Data.Filesystem.FunFS.Util.Serialisable
import Data.Filesystem.FunFS.Util.SizeOf
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

instance Arbitrary BlockGroupDescriptor where
    arbitrary = do
        bgBlockBitmap     <- arbitrary
        bgInodeBitmap     <- arbitrary
        bgInodeTable      <- arbitrary
        bgFreeBlocksCount <- arbitrary
        bgFreeInodesCount <- arbitrary
        bgUsedDirsCount   <- arbitrary
        bgPad             <- arbitrary
        bgReserved        <- vectorOf 12 arbitrary
        return BlockGroupDescriptor {..}

prop_sizeOfBGD :: BlockGroupDescriptor -> Bool
prop_sizeOfBGD bgd = sizeof bgd == sizeOfBGD

prop_encodeBGD :: BlockGroupDescriptor -> Bool
prop_encodeBGD bgd = len == sizeOfBGD
    where bs  = encode   emptyBGD
          len = fromIntegral $ B.length bs

prop_decodeBGD :: BlockGroupDescriptor -> Bool
prop_decodeBGD bgd1 = bgd1 == bgd2
    where bs   = encode bgd1
          bgd2 = decode bs

testBlockGroupDescriptor = do
    quickCheck prop_sizeOfBGD
    quickCheck prop_encodeBGD
    quickCheck prop_decodeBGD
