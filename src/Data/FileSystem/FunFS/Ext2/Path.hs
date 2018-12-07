{- |
 - Module      : Data.FileSystem.FunFS.Ext2.Path
 - Description : Delimited file path.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.Path ( Path
                                       , elems
                                       , sep
                                       , empty
                                       , makePath
                                       , buildPath
                                       , join
                                       , join2
                                       , joinAll
                                       , root
                                       , tree
                                       , dirs
                                       , leaf
                                       , parent
                                       , isEmpty
                                       , isRoot
                                       , isLeaf
                                       , isTree
                                       ) where

import Data.List
import Prelude hiding ((/),concat)

-- | Path .
newtype Path = Path { elems :: [String] }
               deriving (Eq)

-- | The path separator.
sep :: Char
sep = '/'

-- | The empty path.
empty :: Path
empty = Path []

-- | Make a path from a string.
makePath :: String -> Path
makePath = buildPath . filter (/="") . map (dropWhile (=='/')) . groupBy (\_ b -> b /= sep)

-- | Build a path from separate elements.
buildPath :: [String] -> Path
buildPath = Path

-- | Concatenate two paths.
join :: Path -> Path -> Path
join p1 p2 = Path $ es1 ++ es2
    where es1 = elems p1
          es2 = elems p2

-- | Concatenate two paths, one of which is a string.
join2 :: Path -> String -> Path
join2 p1 p2 = join p1 $ makePath p2

-- | Concatenate several paths.
joinAll :: [Path] -> Path
joinAll []     = empty
joinAll (p:ps) = foldl join p ps

-- | Get the root element of a path.
root :: Path -> String
root = head . elems

-- | Get the leaf element of a path.
leaf :: Path -> String
leaf = last . elems

-- | Get the parent of the leaf element of a path.
parent :: Path -> String
parent = last . init . elems

-- | Get all the elements of a path except the root.
tree :: Path -> Path
tree = Path . tail . elems

-- | Get all the elements of a path except the leaf.
dirs :: Path -> Path
dirs = Path . init . elems

-- | Indicates whether a path is empty.
isEmpty :: Path -> Bool
isEmpty = null . elems

isRoot :: Path -> Bool
isRoot = null . tail . elems

isLeaf :: Path -> Bool
isLeaf = isRoot

isTree :: Path -> Bool
isTree = not . isRoot

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Path where
    show (Path []) = [sep]
    show (Path xs) = concatMap (\x -> sep:x) xs
