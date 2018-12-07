{- |
 - Module      : Data.FileSystem.FunFS.API.HandleInfo
 - Description : Handle info. This module exists primarily to break a cylic dependency between
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.HandleInfo ( HandleInfo(..)
                                            , makeHandleInfo
                                            ) where
--
import Data.FileSystem.FunFS.API.NodeType
import Data.FileSystem.FunFS.API.OpenMode
import Data.FileSystem.FunFS.API.Permission
import Data.FileSystem.FunFS.API.User
import Data.FileSystem.FunFS.Util.Strings

import qualified Data.FileSystem.FunFS.Ext2 as Ext2

-- | Describes an open or directory.
data HandleInfo = HandleInfo { hNode        :: Ext2.Node  -- ^ The filesystem node.
                             , hType        :: NodeType   -- ^ The node type.
                             , hOpenMode    :: OpenMode   -- ^ The opening mode.
                             , hPermissions :: Permission -- ^ The permissions of the handle.
                             , hPosition    :: Int        -- ^ The read/write position.
                             , hSize        :: Int        -- ^ The size of the file/directory in bytes.
                             }
                  deriving (Eq)

-- | Create and populate a HandleInfo record. Also performs permissions checks.
makeHandleInfo :: User -> Ext2.Node -> OpenMode -> HandleInfo
makeHandleInfo user node mode
    | wantRead  && not canRead  = error eNoRead
    | wantWrite && not canWrite = error eNoWrite
    | _type == SpecialNode      = error eIsSpecial
    | otherwise                 = info
    where wantRead  = mode == ReadOnly  || mode == ReadWrite
          wantWrite = mode == WriteOnly || mode == ReadWrite || mode == Append
          perms     = getPermissions user node
          canRead   = readable       perms
          canWrite  = writeable      perms
          _type     = Ext2.nodeType  node
          size      = Ext2.nodeSize  node
          info      = HandleInfo { hNode        = node
                                 , hType        = _type
                                 , hOpenMode    = mode
                                 , hPermissions = perms
                                 , hPosition    = if   mode == Append
                                                  then size
                                                  else 0
                                 , hSize        = size
                                 }

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show HandleInfo where
    show info = concat [ "ino=",    show $ Ext2.inum    $ hNode info
                       , "  (",     show $ hOpenMode    info, ")"
                       , "  [",     show $ hPermissions info, "]"
                       , "  pos=",  show $ hPosition    info
                       , "  size=", show $ hSize        info
                       ]
