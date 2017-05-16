module Main where
--
import TestBlock
import TestBlockGroupDescriptor
import TestInode
import TestLinkedDirectory
import TestPath
import TestSuperBlock
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "# Block"
    testBlock
    putStrLn "# BlockGroupDescriptor"
    testBlockGroupDescriptor
    putStrLn "# Inode"
    testInode
    putStrLn "# LinkedDirectory"
    testLinkedDirectory
    putStrLn "# Path"
    testPath
    putStrLn "# SuperBlock"
    testSuperBlock
