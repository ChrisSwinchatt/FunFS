{- |
 - Module      : Data.FileSystem.FunFS.Ext2.NodeType
 - Description : Node type.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.NodeType ( NodeType(..)
                                           , isFileNode
                                           , isDirNode
                                           , isSpecialNode
                                           ) where

-- | Node type.
data NodeType = FileNode    -- ^ A regular file.
              | DirNode     -- ^ A directory. Contains references to other nodes.
              | SpecialNode -- ^ A special node such as a device or symbolic link.
              deriving (Enum,Eq,Ord)


-- | Indicates whether the node type is FileNode.
isFileNode :: NodeType -> Bool
isFileNode = (FileNode ==)

-- | Indicates whether the node type is DirectNode.
isDirNode :: NodeType -> Bool
isDirNode = (DirNode ==)

-- | Indicates whether the node type is SpecialNode.
isSpecialNode :: NodeType -> Bool
isSpecialNode = (SpecialNode ==)

instance Show NodeType where
    show FileNode    = "file"
    show DirNode     = "directory"
    show SpecialNode = "special"
