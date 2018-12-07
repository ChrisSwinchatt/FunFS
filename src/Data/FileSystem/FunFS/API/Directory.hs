{- |
 - Module      : Data.FileSystem.FunFS.API.Directory
 - Description : Directory API.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.Directory ( Directory
                                           , DirEntry
                                           , getEntryName
                                           , getEntryNode
                                           , openDir
                                           , openDirP
                                           , openDirAs
                                           , openDirAsP
                                           , closeDir
                                           , readDir
                                           , mapDir
                                           , getDirHandle
                                           , readDirToEnd
                                           , getDirSize
                                           ) where
--
import Data.FileSystem.FunFS.API.Handle
import Data.FileSystem.FunFS.API.FileSystem
import Data.FileSystem.FunFS.API.OpenMode
import Data.FileSystem.FunFS.API.Path
import Data.FileSystem.FunFS.API.Seek
import Data.FileSystem.FunFS.API.User
import Data.FileSystem.FunFS.Util.Strings

import qualified Data.FileSystem.FunFS.Ext2 as Ext2

-- | An open directory handle. Composes Ext2.Directory and Handle
data Directory = Directory { e2Dir   :: Ext2.Directory -- ^ Ext2 directory.
                           , handle  :: Handle         -- ^ Handle.
                           , entries :: [DirEntry]     -- ^ Directory entries.
                           }

-- | A directory entry. Associates a node with the name given it by its parent directory.
newtype DirEntry = DirEntry { e2Entry :: Ext2.DirEntry }

-- | Get the name associated with a directory entry.
getEntryName :: DirEntry -> String
getEntryName = Ext2.getEntryName . e2Entry

-- | Get the node associated with a directory entry.
getEntryNode :: DirEntry -> Ext2.Node
getEntryNode = Ext2.getEntryNode . e2Entry

-- | Open a directory as superuser.
openDir :: String       -- ^ A path which names a directory.
        -> OpenMode     -- ^ The directory open mode (read/write).
        -> FS Directory -- ^ The opened directory in FS.
openDir = openDirAs superuser

-- | Open a directory as superuser.
openDirP :: Path         -- ^ A path which names a directory.
         -> OpenMode     -- ^ The directory open mode (read/write).
         -> FS Directory -- ^ The opened directory in FS.
openDirP = openDirAsP superuser

-- | Open a directory as a given user.
openDirAs :: User         -- ^ The user whose permissions will be used when opening the directory.
          -> String       -- ^ A path which names a directory.
          -> OpenMode     -- ^ The directory open mode (read/write).
          -> FS Directory -- ^ The opened directory in FS.
openDirAs = (. makePath) . openDirAsP

-- | Open a directory as a given user.
openDirAsP :: User         -- ^ The user whose permissions will be used when opening the directory.
           -> Path         -- ^ A path which names a directory.
           -> OpenMode     -- ^ The directory open mode (read/write).
           -> FS Directory -- ^ The opened directory in FS.
openDirAsP user path mode = do
    fs <- get
    let rootDir = rootDirectory       fs
        node    = Ext2.getChildByPath rootDir path
        dir     = Ext2.makeDirectory  node
        es      = Ext2.getEntries     dir
        -- FIXME: The next line forces evaluation of the whole directory. THis being the case, we may as well just
        size    = length              es
    h <- openHWithSize user node mode size
    return Directory { e2Dir = dir, handle = h, entries = map DirEntry es }

-- | Close a directory.
closeDir :: Directory -> FS ()
closeDir = closeH . handle

-- | Change the read/write position of a directory.
seekDir :: Seek   -- ^ The seek position to offset from - start, current or end.
        -> Int       -- ^ How many entries forwards to seek.
        -> Directory -- ^ The directory to seek in.
        -> FS ()     -- ^ The updated FS.
seekDir sp i dir = seekH sp i $ handle dir

-- | Read the next directory entry from a directory. It is an error to attempt to read more than the number of entries
-- in the directory, which can be checked with getDirSize.
readDir :: Directory -> FS DirEntry
readDir dir = do
    seekDir CurPos 1 dir
    maybePos <- getDirPosition dir
    case maybePos of
        Nothing  -> error  eNotOpen
        Just pos -> return DirEntry { e2Entry = Ext2.getEntries (e2Dir dir) !! pos }

-- | Map a function over all the entries in a directory.
-- NB: The reading position is not altered (unless the function argument to mapDir causes it) so thatthe code:
-- @
--    entry1 <- readDir dir
--    mapDir f dir
--    entry2 <- readDir
-- @
-- Has the same effect as if mapDir was removed, i.e. entry1 and entry2 are adjacent entries.
mapDir :: (DirEntry -> FS a) -> Directory -> FS [a]
mapDir f = mapM f . entries

-- | Read all the entries of a directory into a list.
readDirToEnd :: Directory -> FS [DirEntry]
readDirToEnd = mapDir return

-- | Get the handle associated with a directory.
getDirHandle :: Directory -> FS Handle
getDirHandle = return . handle

-- | Get the number of entries in a directory.
getDirSize :: Directory -> FS (Maybe Int)
getDirSize = getHSize . handle

-- | Get the read/write position of a directory.
getDirPosition :: Directory -> FS (Maybe Int)
getDirPosition = getHPosition . handle

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show DirEntry where
    show = show . e2Entry

instance Show Directory where
    show (Directory _ h es) = show h ++ unlines (map show es)
