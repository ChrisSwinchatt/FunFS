{- |
 - Module      : Data.FileSystem.FunFS.API.Permission
 - Description : File permissions.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.Permission ( Permission(..)
                                            , getPermissions
                                            ) where
--
import Data.FileSystem.FunFS.API.User

import qualified Data.FileSystem.FunFS.Ext2 as Ext2

-- | File permission.
data Permission = Permission { readable   :: Bool -- ^ Whether the file is readable.
                             , writeable  :: Bool -- ^ Whether the file is writeable.
                             , executable :: Bool -- ^ Whether the file is executable.
                             }
                             deriving (Eq)

{- | Get permissions of a node wrt a given user. In other words, tests whether a user is allowed to read, write and
 - execute a node and wraps this in a Permission.
 -}
getPermissions :: User -> Ext2.Node -> Permission
getPermissions user node = Permission { readable   = Ext2.isReadable   user node
                                      , writeable  = Ext2.isWriteable  user node
                                      , executable = Ext2.isExecutable user node
                                      }

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Permission where
    show p = foldr (:) "" [ if readable   p then 'r' else '-'
                          , if writeable  p then 'w' else '-'
                          , if executable p then 'x' else '-'
                          ]
