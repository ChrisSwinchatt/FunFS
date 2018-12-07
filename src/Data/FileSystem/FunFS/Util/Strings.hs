{- |
 - Module      : Data.FileSystem.FunFS.Util.Strings
 - Description : String constants and operations.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.Strings where

import Data.Char
import Data.Word

-- Error messages intended for programmers.
eNegativeIndex = "List index must be non-negative"
eLargeIndex    = "List index must be smaller than the size of the list"
eListTooSmall  = "Cannot create a list of size < 1"
eBufferOverrun = "Buffer not large enough for attempted I/O operation"
eBadInode      = "Inode does not exist"
eBadOffset     = "Attempted to read past the end of a file"
eEmptyList     = "Empty list"
eNotOpen       = "Handle does not refer to an open file or directory"

-- Error messages intended for users. These should be more descriptive and less technical.
eDeviceTooSmall  = "Device is too small to contain a filesystem"
eBadSuperBlock   = "Device does not contain a valid Ext2 filesystem or the filesystem is corrupt. See fsck(8)"
eBadVersion      = "FileSystem cannot be mounted because its version is not compatible"
eNeedFsck        = "FileSystem was not cleanly unmounted and may contain errors. See fsck(8)"
eFeatureIncompat = "FileSystem cannot be mounted because it requires features which are not supported"
eIsADirectory    = "Path names a directory"
eNotADirectory   = "Path does not name a directory"
eIsSpecial       = "Expected a regular file or a directory, but requested node is a special file"
eNotFound        = "Could not find the requested file or directory"
eNoRead          = "User does not have permission to read the file"
eNoWrite         = "User does not have permission to write the file"
eNoExec          = "User does not have permission to execute the file"
eInUse           = "FileSystem is in use"
eBadF            = "Bad file descriptor"
eBadSeek         = "Illegal seek"

-- | Convert an array 's' of length 'n' into a string.
arrayToString :: [Word8] -> Int -> String
arrayToString s n = take n $ map (chr . fromIntegral) s

eNegativeIndex :: String
eLargeIndex :: String
eListTooSmall :: String
eBufferOverrun :: String
eBadInode :: String
eBadOffset :: String
eEmptyList :: String
eNotOpen :: String
eDeviceTooSmall :: String
eBadSuperBlock :: String
eBadVersion :: String
eNeedFsck :: String
eFeatureIncompat :: String
eIsADirectory :: String
eNotADirectory :: String
eIsSpecial :: String
eNotFound :: String
eNoRead :: String
eNoWrite :: String
eNoExec :: String
eInUse :: String
eBadF :: String
eBadSeek :: String
