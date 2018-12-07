{- |
 - Module      : Data.FileSystem.FunFS.Ext2.Directory
 - Description : Directories.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.Directory ( Directory
                                            , makeDirectory
                                            , getRootDirectory
                                            , getEntries
                                            , getNames
                                            , getChildren
                                            , getChildByName
                                            , getChildByPath
                                            ) where
--
import Data.FileSystem.FunFS.Ext2.LowLevel.LinkedDirectory
import Data.FileSystem.FunFS.Ext2.DirEntry
import Data.FileSystem.FunFS.Ext2.Path
import Data.FileSystem.FunFS.Ext2.Node
import Data.FileSystem.FunFS.Ext2.Volume
import Data.FileSystem.FunFS.Util.Strings
import Data.Maybe

-- | Directory - a node which contains other nodes.
data Directory = Directory { base       :: Node
                           , getEntries :: [DirEntry]
                           }

-- | Make a directory from a node. The node must be of type Directory.
makeDirectory :: Node -> Directory
makeDirectory node
    | isDirectory node = Directory node entries
    | otherwise        = error     $ concat [eNotADirectory,"\n",show node]
    where   vol     = volume node
            size    = nodeSize node
            records = map fst $ tail $ iterate getRecord (Just emptyLinkedDirectory,0)
            entries = mapMaybe (makeDirEntry vol . fromJust) $ takeWhile isJust records
            getRecord (_,off) = if   off < size
                                then let ld  = getObject node (off,linkedDirectorySize)
                                         len = fromIntegral $ dRecLen ld
                                     in  (Just ld,off + len)
                                else (Nothing,0)

-- | Get the root directory.
getRootDirectory :: Volume -> Directory
getRootDirectory = makeDirectory . getRootNode

-- | Get a list of a directory's children (subordinate nodes).
getChildren :: Directory -> [Node]
getChildren = map getEntryNode . getEntries

-- | Get a list of the names of a directory's children.
getNames :: Directory -> [String]
getNames = map getEntryName . getEntries

-- | Get the child node named by 's' in 'dir' if it exists or Nothing if it does not.
getChildByName :: Directory -> String -> Maybe Node
getChildByName dir s = let matches = map getEntryNode . filter (\e -> getEntryName e == s) $ getEntries dir
                       in  if   null matches
                           then Nothing
                           else Just $ head matches

-- | Scan the directory tree for a node named by 'path'. It is an error if the node does not exist.
-- Paths are assumed to be relative from the directory given. An empty path or one containing only "." will match the
-- given directory.
getChildByPath :: Directory -> Path -> Node
getChildByPath dir path
    | not $ isDirectory $ base dir = error $ concat [eNotADirectory,": ",root path]
    | isEmpty path                 = base dir
    | otherwise                    = let name = root path
                                     in  case getChildByName dir name of
                                            Nothing                 -> error $ concat [eNotFound,": ",name]
                                            Just child
                                                | isRoot      path  -> child
                                                | isDirectory child -> getChildByPath (makeDirectory child) $ tree path
                                                | otherwise         -> error $ concat [ eNotADirectory, ": "
                                                                                      , name,           ": "
                                                                                      , show child
                                                                                      ]
------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Directory where
    show dir = concat [ "{",show $ base dir,"} contains ", show n, " entries:\n"
                      , concatMap (\x -> show x ++ "\n") es
                      ]
        where es = getEntries dir
              n  = length     es
