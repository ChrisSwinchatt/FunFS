{-# LANGUAGE RecordWildCards #-}
module TestSuperBlock where
--
import Data.FileSystem.FunFS.Ext2.LowLevel.SuperBlock
import Data.FileSystem.FunFS.Util.Serialisable
import Data.FileSystem.FunFS.Util.SizeOf
import Data.Word
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

instance Arbitrary SuperBlock where
    arbitrary = do
        sInodesCount         <- arbitrary
        sBlocksCount         <- arbitrary
        sRBlocksCount        <- arbitrary
        sFreeBlocksCount     <- arbitrary
        sFreeInodesCount     <- arbitrary
        sFirstDataBlock      <- arbitrary
        sLogBlockSize        <- arbitrary
        sLogFragSize         <- arbitrary
        sBlocksPerGroup      <- arbitrary
        sFragsPerGroup       <- arbitrary
        sInodesPerGroup      <- arbitrary
        sMtime               <- arbitrary
        sWtime               <- arbitrary
        sMountCount          <- arbitrary
        sMaxMountCount       <- arbitrary
        sMagic               <- arbitrary
        sState               <- arbitrary
        sErrors              <- arbitrary
        sMinorRevLevel       <- arbitrary
        sLastCheck           <- arbitrary
        sCheckInterval       <- arbitrary
        sCreatorOs           <- arbitrary
        sRevLevel            <- arbitrary
        sDefResUid           <- arbitrary
        sDefResGid           <- arbitrary
        sFirstIno            <- arbitrary
        sInodeSize           <- arbitrary
        sBlockGroupNr        <- arbitrary
        sFeatureCompat       <- arbitrary
        sFeatureIncompat     <- arbitrary
        sFeatureRoCompat     <- arbitrary
        sUuid                <- vectorOf 16 arbitrary
        sVolumeName          <- vectorOf 16 arbitrary
        sLastMounted         <- vectorOf 64 arbitrary
        sAlgoBitmap          <- arbitrary
        sPreallocBlocks      <- arbitrary
        sPreallocDirBlocks   <- arbitrary
        alignment            <- arbitrary
        sJournalUuid         <- vectorOf 16 arbitrary
        sJournalInum         <- arbitrary
        sJournalDev          <- arbitrary
        sLastOrphan          <- arbitrary
        sHashSeed            <- vectorOf 4 arbitrary
        sDefHashVersion      <- arbitrary
        padding              <- vectorOf 3 arbitrary
        sDefaultMountOptions <- arbitrary
        sFirstMetaBg         <- arbitrary
        return SuperBlock {..}

prop_sizeOfSuperBlock :: SuperBlock -> Bool
prop_sizeOfSuperBlock sb = sizeof sb == sizeof defaultSuperBlock

prop_encodeSuperBlock :: SuperBlock -> Bool
prop_encodeSuperBlock sb = len == size && size == sizeof defaultSuperBlock
    where bs   = encode sb
          len  = B.length bs
          size = sizeof sb

prop_decodeSuperBlock :: SuperBlock -> Bool
prop_decodeSuperBlock sb1 = sb1 == sb2
    where bs  = encode sb1
          sb2 = decode bs

testSuperBlock :: IO ()
testSuperBlock = do
    quickCheck prop_sizeOfSuperBlock
    quickCheck prop_encodeSuperBlock
    quickCheck prop_decodeSuperBlock
