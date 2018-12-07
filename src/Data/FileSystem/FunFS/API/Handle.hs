{- |
 - Module      : Data.FileSystem.FunFS.API.Handle
 - Description : FileSystem I/O handle.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.Handle ( Handle
                                        , openH
                                        , openHWithSize
                                        , closeH
                                        , seekH
                                        , readH
                                        , isHOpen
                                        , isHClosed
                                        , getHInfo
                                        , getHType
                                        , getHNode
                                        , getHOpenMode
                                        , getHPermissions
                                        , getHPosition
                                        , getHSize
                                        ) where
--
import Data.FileSystem.FunFS.API.NodeType
import Data.FileSystem.FunFS.API.FileSystem
import Data.FileSystem.FunFS.API.HandleInfo
import Data.FileSystem.FunFS.API.OpenMode
import Data.FileSystem.FunFS.API.Permission
import Data.FileSystem.FunFS.API.Seek
import Data.FileSystem.FunFS.API.User
import Data.FileSystem.FunFS.Util.Strings

import qualified Data.ByteString.Lazy       as B
import qualified Data.FileSystem.FunFS.Ext2 as Ext2

{- | A FileSystem handle - references an open file or directory within the filesystem. Essentially equivalent to POSIX
 -  file descriptors (in fact it is exactly equal).
 -}
newtype Handle = Handle Int
                 deriving (Eq)

-- | Open a handle to a filesystem node.
openH :: User       -- ^ Use the permissions of a user to open the file.
      -> Ext2.Node  -- ^ The node to which the handle should refer.
      -> OpenMode   -- ^ The opening mode.
      -> FS Handle  -- ^ An open handle to the node wrapped in updated FS.
openH user node mode = do
    let hi = makeHandleInfo user node mode
    fd <- appendFDHandleInfo hi
    return $ Handle fd

-- | Open a handle, overriding the size reported by the node.
-- This is a workaround to allow directories to report their size as the number of entries they contain, rather than the
-- sum of the sizes of the entries.
openHWithSize :: User -> Ext2.Node -> OpenMode -> Int -> FS Handle
openHWithSize user node mode size = do
    let hi = makeHandleInfo user node mode
    fd <- appendFDHandleInfo  hi { hSize = size }
    return $ Handle fd

-- | Close a handle.
closeH :: Handle -> FS ()
closeH (Handle fd) = do
    _ <- updateFDHandleInfo fd (const Nothing)
    return ()

-- | Update the read/write position of a handle.
seekH :: Seek -> Int -> Handle -> FS ()
seekH sp offset (Handle fd) = maybeHI (error eBadSeek) doSeek fd
    where doSeek hi = do
            let pos  = hPosition  hi
                size = hSize      hi
                pos' = computeSeek sp (0,pos,size) offset
            updatePosition fd pos'
            return ()

-- | Read data from a handle.
readH :: Handle -> Int -> FS B.ByteString
readH (Handle fd) n = maybeHI (error eBadF) doRead fd
    where doRead hi = do
            let pos   = hPosition hi
                size  = hSize     hi
                node  = hNode     hi
                count = min       n (size - pos)
                bytes = Ext2.getBytes' node (fromIntegral pos,fromIntegral count)
            updatePosition fd $ pos + count
            return bytes
{-
writeH :: Handle -> B.ByteString -> FS Int
writeH (Handle fd) bs = maybeHI (error eBadF) doWrite fd
    where doWrite hi = do
        let pos   = hPosition hi
            size  = hSize     hi
            node  = hNode     hi
            len   = B.length  bs
            count = min       len (size - pos)
        Ext2.putBytes node bs (fromIntegral pos,fromIntegral count)
-}
-- | Indicates whether the handle is open.
isHOpen :: Handle -> FS Bool
isHOpen (Handle fd) = maybeHI False (const $ return True) fd

-- | Indicates whether the handle is closed.
isHClosed :: Handle -> FS Bool
isHClosed = fmap not . isHOpen

-- | Get the HandleInfo associated with a Handle. If the handle is closed, Nothing is returned.
getHInfo :: Handle -> FS (Maybe HandleInfo)
getHInfo (Handle fd) = getFDHandleInfo fd

-- | Get the NodeType associated with a Handle. If the handle is closed, Nothing is returned.
getHType :: Handle -> FS (Maybe NodeType)
getHType (Handle fd) = maybeHI Nothing (return . Just . hType) fd

-- | Get the Node associated with a Handle. If the handle is closed, Nothing is returned.
getHNode :: Handle -> FS (Maybe Ext2.Node)
getHNode (Handle fd) = maybeHI Nothing (return . Just . hNode) fd

-- | Get the OpenMode associated with a Handle. If the handle is closed, Nothing is returned.
getHOpenMode :: Handle -> FS (Maybe OpenMode)
getHOpenMode (Handle fd) = maybeHI Nothing (return . Just . hOpenMode) fd

-- | Get the Permission associated with a Handle. If the handle is closed, Nothing is returned.
getHPermissions :: Handle -> FS (Maybe Permission)
getHPermissions (Handle fd) = maybeHI Nothing (return . Just . hPermissions) fd

-- | Get the position associated with a Handle. If the handle is closed, Nothing is returned.
getHPosition :: Handle -> FS (Maybe Int)
getHPosition (Handle fd) = maybeHI Nothing (return . Just . hPosition) fd

-- | Get the size associated with a Handle. If the handle is closed, Nothing is returned.
getHSize :: Handle -> FS (Maybe Int)
getHSize (Handle fd) = maybeHI Nothing (return . Just . hSize) fd

------------------------------------------------------------------------------------------------------------------------
-- INTERNAL
-- | Set position for `fd` to `pos`.
-- NB: This function is 'dumb' and does not check values of `fd` or `pos` (though `updateFDHandleInfo` checks `fd`)
updatePosition :: Int -> Int -> FS ()
updatePosition fd pos = do
    _ <- updateFDHandleInfo fd f
    return ()
    where   f Nothing   = Nothing
            f (Just hi) = Just $ hi { hPosition = pos }
------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Handle where
    show (Handle fd) = "Handle " ++ show fd
