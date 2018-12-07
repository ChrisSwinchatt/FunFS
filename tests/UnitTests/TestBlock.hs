module TestBlock where
--
import Data.FileSystem.FunFS.Ext2.LowLevel.Block
import Data.Int
import Data.Word
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

data TestBlock = TestBlock Block Int64
                 deriving (Eq,Show)

prop_peek :: TestBlock -> Bool
prop_peek (TestBlock b i) = w1 == w2
    where bs  = bytes   b
          w1  = B.index bs i
          w2  = peek    b  i

prop_poke :: TestBlock -> Word8 -> Bool
prop_poke (TestBlock b1 i) w1 = w1 == w2
    where b2  = poke    b1         i w1
          w2  = B.index (bytes b2) i

testBlock = do
    putStrLn "## peek"
    quickCheck prop_peek
    putStrLn "## poke"
    quickCheck prop_poke

instance Arbitrary TestBlock where
    arbitrary = do
        xs <- listOf1 arbitrary
        let n = length xs
        i  <- choose (0,n - 1)
        return $ TestBlock (makeBlock (B.pack xs) $ fromIntegral n) $ fromIntegral i
