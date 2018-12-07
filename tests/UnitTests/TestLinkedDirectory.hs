{-# LANGUAGE RecordWildCards #-}
module TestLinkedDirectory where
--
import Data.FileSystem.FunFS.Ext2.LowLevel.LinkedDirectory
import Data.FileSystem.FunFS.Util.Serialisable
import Data.FileSystem.FunFS.Util.SizeOf
import Data.Word
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

instance Arbitrary LinkedDirectory where
    arbitrary = do
        dInode   <- arbitrary
        dRecLen  <- arbitrary
        dNameLen <- arbitrary
        dName    <- arbitrary
        return LinkedDirectory {..}

prop_sizeOfLinkedDirectory :: LinkedDirectory -> Bool
prop_sizeOfLinkedDirectory ld = sizeof ld == linkedDirectorySize

prop_encodeLinkedDirectory :: LinkedDirectory -> Bool
prop_encodeLinkedDirectory ld = len == size && size == linkedDirectorySize
    where bs   = encode ld
          len  = B.length bs
          size = sizeof ld

prop_decodeLinkedDirectory :: LinkedDirectory -> Bool
prop_decodeLinkedDirectory ld1 = ld1 == ld2
    where bs  = encode ld1
          ld2 = decode bs

testLinkedDirectory :: IO ()
testLinkedDirectory = do
    quickCheck prop_sizeOfLinkedDirectory
    quickCheck prop_encodeLinkedDirectory
    quickCheck prop_decodeLinkedDirectory
