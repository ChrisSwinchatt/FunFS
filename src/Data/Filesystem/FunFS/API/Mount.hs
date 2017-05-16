{- |
 - Module      : Data.Filesystem.FunFS.API.Mount
 - Description : Mount/umount filesystem.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.API.Mount ( mount
                                       , unmount
                                       , forceUnmount
                                       , withMount
                                       ) where
--
import Data.Filesystem.FunFS.API.Filesystem
import Data.Filesystem.FunFS.Util.Strings

import qualified Data.ByteString.Lazy       as B
import qualified Data.Filesystem.FunFS.Ext2 as Ext2

-- | Mount a filesystem.
mount :: String         -- ^ A path to the file or device *in the *host* FS* which contains a filesystem.
      -> IO Filesystem  -- ^ The filesystem within IO. Use 'sync' to use the filesystem in FS.
mount path = do
    bytes <- B.readFile path
    let vol  = Ext2.mountVolume      bytes
        root = Ext2.getRootDirectory vol
    return Filesystem { volume        = vol
                      , handles       = []
                      , rootDirectory = root
                      }

-- | Unmount a filesystem. The unmount will fail if there are open handles.
unmount :: FS ()
unmount = do
    b <- isInUse
    if   b
    then error eInUse
    else put   NotMounted

-- | Unmount a filesystem forcibly. Invalidates all open handles.
forceUnmount :: FS ()
forceUnmount = do
    put    NotMounted
    return ()

-- | Mount a filesystem and apply a filesystem action to it, returning the result in IO.
withMount :: String -> FS a -> IO a
withMount path s = do
    fs <- mount path
    sync s fs
