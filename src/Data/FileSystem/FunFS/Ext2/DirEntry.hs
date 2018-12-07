{- |
 - Module      : Data.FileSystem.FunFS.Ext2.DirEntry
 - Description : Directory entries.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.DirEntry ( DirEntry
                                           , makeDirEntry
                                           , getEntryNode
                                           , getEntryName
                                           ) where
--
import Data.FileSystem.FunFS.Ext2.LowLevel.LinkedDirectory
import Data.FileSystem.FunFS.Ext2.Node
import Data.FileSystem.FunFS.Ext2.Volume
import Data.FileSystem.FunFS.Util.Strings

-- | Directory entry.
data DirEntry = DirEntry { getEntryNode :: Node
                         , getEntryName :: String
                         }
                         deriving (Eq)

-- | Parse a directory entry from a LinkedDirectory. Returns Nothing if the entry is invalid. This is not necessarily
-- (or even usually) an error because dummy entries with inode 0 are sometimes used to pad records across block
-- boundaries.
makeDirEntry :: Volume -> LinkedDirectory -> Maybe DirEntry
makeDirEntry vol ld
    | num == 0  = Nothing
    | otherwise = let node = getNodeByInum vol num
                      len  = fromIntegral $ dNameLen ld
                      name = dName ld
                  in  Just $ DirEntry node $ arrayToString name len
    where num = fromIntegral $ dInode ld

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show DirEntry where
    show entry = concat [show (getEntryNode entry), ": ", getEntryName entry]
