{- |
 - Module      : Data.Filesystem.FunFS.API.Filesystem
 - Description : Filesystem state and FS monad.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.API.Filesystem ( Filesystem(..)
                                            , FS
                                            , get
                                            , put
                                            , sync
                                            , appendFDHandleInfo
                                            , updateFDHandleInfo
                                            , isInUse
                                            , getStatistics
                                            , openHandles
                                            , getFDHandleInfo
                                            , haveFDHandleInfo
                                            , maybeHI
                                            ) where
--
import Control.Monad.Trans.State.Strict
import Data.Filesystem.FunFS.API.HandleInfo
import Data.Filesystem.FunFS.Util.Strings
import Data.Maybe

import qualified Data.Filesystem.FunFS.Ext2 as Ext2

-- | Filesystem.
data Filesystem = NotMounted -- ^ An invalid/unmounted filesystem.
                -- | A mounted filesystem.
                | Filesystem { volume        :: Ext2.Volume        -- ^ Ext2 volume.
                             , handles       :: [Maybe HandleInfo] -- ^ List of handles.
                             , rootDirectory :: Ext2.Directory     -- ^ The root directory.
                             }

-- | FS monad.
type FS a = State Filesystem a

-- | Apply a filesystem action to a mounted filesystem and return the result in IO. Due to lazy evaluation, filesystem
-- operations are not evaluated until 'sync' is called.
sync :: FS a -> Filesystem -> IO a
sync = (return .) . evalState

-- | Indicates whether the filesystem is in use, i.e. if there are open handles.
isInUse :: FS Bool
isInUse = do
    fs <- get
    let oh = openHandles fs
    return $ not $ null oh

-- | Get printable statistics about the filesystem.
getStatistics :: FS String
getStatistics = do
    fs <- get
    return $ show fs

-- | Get a list of currently open handles.
openHandles :: Filesystem -> [HandleInfo]
openHandles = catMaybes . handles

-- | Append the HandleInfo for a newly opened handle to a filesystem's list of open handles and get a file descriptor
-- unique to it. It is NOT an error if there is already a handle to the same node, though it may cause strange
-- behaviour.
appendFDHandleInfo :: HandleInfo -> FS Int
appendFDHandleInfo hi = do
    fs <- get
    let hs  = handles fs
        fd  = length  hs
        hs' = hs ++ [Just hi]
    put fs { handles = hs' }
    return fd

-- | Update the HandleInfo referenced by a file descriptor by applying a function to it and return the result.
-- It is an error if the file descriptor refers to a closed or otherwise invalid handle.
updateFDHandleInfo :: Int -> (Maybe HandleInfo -> Maybe HandleInfo) -> FS (Maybe HandleInfo)
updateFDHandleInfo fd f
    | fd < 0    = error msg
    | otherwise = do
        fs <- get
        let hs  = handles fs
            len = length  hs
        if   fd >= len
        then error msg
        else let (lhs,rhs) = splitAt fd hs
                 hi'       = f $ head rhs
                 hs'       = lhs ++ hi':tail rhs
             in  do
                put fs { handles = hs' }
                return hi'
    where msg = concat [eBadF,": ",show fd]

-- | Get the HandleInfo referenced by a file descriptor. If the handle is closed, the result will be Nothing.
getFDHandleInfo :: Int -> FS (Maybe HandleInfo)
getFDHandleInfo fd
    | fd < 0    = error msg
    | otherwise = do
        fs <- get
        let hs = handles fs
        if   fd > length hs
        then error  msg
        else return $ hs !! fd
    where msg = concat [eBadF,": ",show fd]

-- | Indicates whether a file descriptor is associated with an open handle.
haveFDHandleInfo :: Int -> FS Bool
haveFDHandleInfo = maybeHI True (const $ return False)

-- | Helper for functions which execute conditionally on whether a handle is opened.
maybeHI :: a -> (HandleInfo -> FS a) -> Int -> FS a
maybeHI y f fd = getFDHandleInfo fd >>= maybe (return y) f

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Filesystem where
    show fs = concat [ "Filesystem:\n"
                     , " `- ", show $ length $ openHandles fs, " open files: ", showList (openHandles fs) "\n"
                     , show $ volume fs
                     , "Root directory: "
                     , show $ rootDirectory fs
                     ]
