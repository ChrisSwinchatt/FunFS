{- |
 - Module      : Data.Filesystem.FunFS.Ext2.Node
 - Description : I/O operations on node.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.Ext2.Node ( Node
                                       , Inode
                                       , inum
                                       , inode
                                       , volume
                                       , rootNodeNumber
                                       , getRootNode
                                       , getNodeByInum
                                       , isRootNode
                                       , nodeType
                                       , nodeSize
                                       , isFile
                                       , isDirectory
                                       , isSpecial
                                       , isOwner
                                       , isInGroup
                                       , isReadable
                                       , isWriteable
                                       , isExecutable
                                       , getBlocks
                                       , getBytes
                                       , getBytes'
                                       , getObject
                                       ) where
--
import Data.Filesystem.FunFS.Ext2.LowLevel.Block
import Data.Filesystem.FunFS.Ext2.LowLevel.Inode
import Data.Filesystem.FunFS.Ext2.LowLevel.SuperBlock
import Data.Filesystem.FunFS.Ext2.NodeType
import Data.Filesystem.FunFS.Ext2.User
import Data.Filesystem.FunFS.Ext2.Volume
import Data.Filesystem.FunFS.Util.Serialisable
import Data.Filesystem.FunFS.Util.SizeOf
import Data.Filesystem.FunFS.Util.Strings
import Data.Int
import Data.Word

import qualified Data.ByteString.Lazy as B

-- | A filesystem node. This is a thin wrapper around 'inode'.
data Node = Node { inum   :: Int
                 , inode  :: Inode
                 , volume :: Volume
                 }
                 deriving (Eq)

-- | The root directory is always inode #2.
rootNodeNumber :: Int
rootNodeNumber = 2

-- | The root node.
getRootNode :: Volume -> Node
getRootNode = flip getNodeByInum rootNodeNumber

getNodeByInum :: Volume -> Int -> Node
getNodeByInum vol num = let ino = getInode vol num
                        in  Node num ino vol

-- | Indicates whether a node is the filesystem root node.
-- The root node is a directory with inode number 2 and no parent node.
isRootNode :: Node -> Bool
isRootNode node = inum node == rootNodeNumber

-- | Get a node's type.
nodeType :: Node -> NodeType
nodeType node
    | typeFile      ino = FileNode
    | typeDirectory ino = DirNode
    | otherwise         = SpecialNode
    where ino = inode node

-- | Get the size in bytes of a node.
nodeSize :: (Integral a) => Node -> a
nodeSize = fromIntegral . iSize . inode

-- | Get the number of blocks held by a node.
nodeBlocks :: (Integral a) => Node -> a
nodeBlocks = fromIntegral . iBlocks . inode

-- | Indicates whether a node is a file.
isFile :: Node -> Bool
isFile = isFileNode . nodeType

-- | Indicates whether a node is a directory.
isDirectory :: Node -> Bool
isDirectory = isDirNode . nodeType

-- | Indicates whether a node is a special.
isSpecial :: Node -> Bool
isSpecial = isSpecialNode . nodeType

-- | Indicates whether a user owns a file. Always returns true if the user is root.
isOwner :: User -> Node -> Bool
isOwner user node = user == superuser || userID user == uid
    where uid = fromIntegral $ iUid $ inode node

-- | Indicates whether a user is in the group which owns a file. Always returns true if the user is root.
isInGroup :: User -> Node -> Bool
isInGroup user node = user == superuser || gid `elem` groups user
    where gid = fromIntegral $ iGid $ inode node

-- | Indicates whether a node can be opened for reading.
isReadable :: User -> Node -> Bool
isReadable user node
    | isOwner   user node = ownerRead ino
    | isInGroup user node = groupRead ino
    | otherwise           = otherRead ino
    where ino = inode node

-- | Indicates whether a node can be opened for writing.
isWriteable :: User -> Node -> Bool
isWriteable user node
    | isOwner   user node = ownerWrite ino
    | isInGroup user node = groupWrite ino
    | otherwise           = otherWrite ino
    where ino = inode node

-- | Indicates whether a node can be opened for execution.
isExecutable :: User -> Node -> Bool
isExecutable user node
    | isOwner   user node = ownerExecute ino
    | isInGroup user node = groupExecute ino
    | otherwise           = otherExecute ino
    where ino = inode node


-- | Get a list of the blocks being used by a node.
getBlocks :: Node -> [Block]
getBlocks node
    | isSpecial node = error eIsSpecial
    | otherwise      = take  bcount blocks
    where   vol    = volume       node
            ino    = inode        node
            bcount = fromIntegral $ iBlocks ino
            blist  = blockList    vol
            sb     = superBlock   vol
            size   = getBlockSize sb
            blocks = concat [direct,singly,doubly,triply]
            direct = getBlocks    $ iDirect ino
            singly = getBlocks'   $ getBlock $ iSinglyIndirect ino
            doubly = getBlocks''  $ getBlock $ iDoublyIndirect ino
            triply = getBlocks''' $ getBlock $ iTriplyIndirect ino
            getBlock     = (blist !!) . fromIntegral                    -- Get block from block pointer.
            getBlocks    = map getBlock                                 -- Get blocks from block pointers.
            getBlocks'   = getBlocks . take asPerB . flip peekObjects 0 -- Get blocks from indirect block.
                where asPerB = size `div` sizeof (0 :: Word32)          -- Number of addresses per block.
            getBlocks''  = concatMap getBlocks' . getBlocks'            -- Get blocks from doubly-indirect block.
            getBlocks''' = concatMap getBlocks' . getBlocks''           -- Get blocks from triply-indirect block.

-- | Get a ByteString containing the entire contents of a node.
getBytes :: Node -> B.ByteString
getBytes node = B.take size $ B.concat $ map bytes $ getBlocks node
    where ino   = inode node
          size  = fromIntegral $ iSize ino  -- The size of the node.

-- | Get a ByteString containing a section of a node's data.
getBytes' :: Node -> (Int64,Int64) -> B.ByteString
getBytes' node (m,n) = B.take n $ B.drop m $ getBytes node

-- | Decode an object of given size starting at a given offset within a node.
getObject :: (Serialisable a) => Node -> (Int64,Int64) -> a
getObject node tup = decode $ getBytes' node tup
{-
putBytes :: Node -> B.ByteString -> (Int64,Int64) -> Node
putBytes node bs (m,n) =
    where bytes = getBytes node
          lhs   = take m bytes
-}
------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Node where
    show node = concat ["Inode ", show $ inum node, ": ", show $ inode node]
