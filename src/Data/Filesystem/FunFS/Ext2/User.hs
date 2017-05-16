{- |
 - Module      : Data.Filesystem.FunFS.Ext2.User
 - Description : User.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.Ext2.User ( User(..)
                                       , superuser
                                       ) where
--

-- | User.
data User = User { userID :: Int   -- ^ UID of user.
                 , groups :: [Int] -- ^ GIDs of groups user belongs to.
                 }
                 deriving (Eq)

-- | The superuser, aka 'root'.
superuser :: User
superuser = User { userID = 0
                 , groups = [0]
                 }

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show User where
    show user = concat [ "User {uid=", show $ userID user
                       , ", gid=",     show $ head $ groups user
                       , ", ",         show $ length (groups user) - 1, "additional groups"
                       , "}"
                       ]
