{- |
 - Module      : Data.Filesystem.FunFS.API.File
 - Description : File API.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.API.File ( File
                                      , fOpen
                                      , fOpenP
                                      , fOpenAs
                                      , fOpenAsP
                                      , fClose
                                      , fSeek
                                      , fRead
                                      , fGetBytes
                                      , fGetHandle
                                      , fGetSize
                                      , fGetPosition
                                      ) where
--
import Data.Filesystem.FunFS.API.Handle
import Data.Filesystem.FunFS.API.Filesystem
import Data.Filesystem.FunFS.API.OpenMode
import Data.Filesystem.FunFS.API.Path
import Data.Filesystem.FunFS.API.Seek
import Data.Filesystem.FunFS.API.User
import Data.Filesystem.FunFS.Util.Strings
import Data.Word

import qualified Data.ByteString.Lazy       as B
import qualified Data.Filesystem.FunFS.Ext2 as Ext2

-- | An open fileA handle to an opened regular file.
newtype File = File { handle :: Handle }

-- | Open a file as superuser.
fOpen :: String   -- ^ A path which names a file.
      -> OpenMode -- ^ The file open mode (read/write).
      -> FS File  -- ^ The opened file in FS.
fOpen = fOpenAs superuser

-- | Open a file as superuser.
fOpenP :: Path     -- ^ A path which names a file.
       -> OpenMode -- ^ The file open mode (read/write).
       -> FS File  -- ^ The opened file in FS.
fOpenP = fOpenAsP superuser

-- | Open a file as a given user.
fOpenAs :: User     -- ^ The user whose permissions will be used when opening the file.
        -> String   -- ^ A path which names a file.
        -> OpenMode -- ^ The file open mode (read/write).
        -> FS File  -- ^ The opened file in FS.
fOpenAs = (. makePath) . fOpenAsP

-- | Open a file as a given user.
fOpenAsP :: User      -- ^ The user whose permissions will be used when opening the file.
         -> Path      -- ^ A path which names a file.
         -> OpenMode  -- ^ The file open mode (read/write).
         -> FS File   -- ^ The opened file in FS.
fOpenAsP user path mode = do
    fs <- get
    let rootDir = rootDirectory       fs
        node    = Ext2.getChildByPath rootDir path
    h <- openH user node mode
    if   Ext2.isDirectory node
    then error  $ concat [eIsADirectory,": ",show path]
    else return File { handle = h }

-- | Close a file.
fClose :: File -> FS ()
fClose = closeH . handle

-- | Change the read/write position of a file.
fSeek :: Seek -> Int -> File -> FS ()
fSeek sp i = seekH sp i . handle

-- | Read from a file.
fRead :: File            -- ^ The file to read from.
      -> Int             -- ^ How many bytes to read.
      -> FS B.ByteString -- ^ The resulting ByteString is returned in FS, which is updated.
fRead = readH . handle

-- | Read bytes from a file.
fGetBytes :: File       -- ^ The file to read from.
          -> Int        -- ^ How many bytes to read.
          -> FS [Word8] -- ^ The resulting list is returned in FS, which is updated.
fGetBytes file i = B.unpack <$> fRead file i

-- | Get the handle associated with a file.
fGetHandle :: File -> FS Handle
fGetHandle = return . handle

-- | Get the number of entries in a file.
fGetSize :: File -> FS (Maybe Int)
fGetSize = getHSize . handle

-- | Get the read/write position of a file.
fGetPosition :: File -> FS (Maybe Int)
fGetPosition = getHPosition . handle

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show File where
    show = show . handle
