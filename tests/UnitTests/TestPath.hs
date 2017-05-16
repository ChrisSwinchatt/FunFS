module TestPath where
--
import Data.Filesystem.FunFS.Ext2.Path
import Test.QuickCheck

prop_isEmpty1 :: Bool
prop_isEmpty1 = isEmpty $ makePath ""

prop_isEmpty2 :: Bool
prop_isEmpty2 = not $ isEmpty $ makePath "/foo"

prop_makePath :: Path -> Bool
prop_makePath path = path == makePath (show path)

prop_join :: Path -> Path -> Bool
prop_join p1 p2 = es3 == es1 ++ es2
    where es1 = elems p1
          es2 = elems p2
          p3  = join p1 p2
          es3 = elems p3

testPath :: IO ()
testPath = do
    quickCheck prop_isEmpty1
    quickCheck prop_isEmpty2
    quickCheck prop_makePath
    quickCheck prop_join

instance Arbitrary Path where
    arbitrary = do
        elems <- listOf arbitrary
        return $ buildPath elems
