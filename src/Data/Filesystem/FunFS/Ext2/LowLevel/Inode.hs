{- |
 - Module      : Data.Filesystem.FunFS.Ext2.LowLevel.Inode
 - Description : Inode
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
{-# LANGUAGE TemplateHaskell #-}
module Data.Filesystem.FunFS.Ext2.LowLevel.Inode where
--
import Data.Bits
import Data.Char
import Data.Filesystem.FunFS.Ext2.LowLevel.Struct
import Data.Filesystem.FunFS.Util.Bytes
import Data.Int
import Data.Word
import Numeric

import qualified Data.ByteString.Lazy as B

-- Inode.
struct "Inode"
       [ field "iMode"           [t|Word16|]    -- i_mode (2 bytes)
       , field "iUid"            [t|Word16|]    -- i_uid (2 bytes)
       , field "iSize"           [t|Word32|]    -- i_size (4 bytes)
       , field "iAtime"          [t|Word32|]    -- i_atime (4 bytes)
       , field "iCtime"          [t|Word32|]    -- i_ctime (4 bytes)
       , field "iMtime"          [t|Word32|]    -- i_mtime (4 bytes)
       , field "iDtime"          [t|Word32|]    -- i_dtime (4 bytes)
       , field "iGid"            [t|Word16|]    -- i_gid (2 bytes)
       , field "iLinksCount"     [t|Word16|]    -- i_links_count (2 bytes)
       , field "iBlocks"         [t|Word32|]    -- i_blocks (4 bytes)
       , field "iFlags"          [t|Word32|]    -- i_flags (4 bytes)
       , field "iOsd1"           [t|Word32|]    -- i_osd1 (4 bytes)
       , array "iDirect"         [t|Word32|] 12 -- 12 direct block pointers.
       , field "iSinglyIndirect" [t|Word32|]    -- Singly-indirect block pointer.
       , field "iDoublyIndirect" [t|Word32|]    -- Doubly-indirect block pointer.
       , field "iTriplyIndirect" [t|Word32|]    -- Triply-indirect block pointer.
       , field "iGeneration"     [t|Word32|]    -- i_generation (4 bytes)
       , field "iFileAcl"        [t|Word32|]    -- i_file_acl (4 bytes)
       , field "iDirAcl"         [t|Word32|]    -- i_dir_acl (4 bytes)
       , field "iFaddr"          [t|Word32|]    -- i_faddr (4 bytes)
       , array "iOsd2"           [t|Word8|]  12 -- i_osd2 (12 bytes)
       ]
       ["Eq"]

-- The number of direct block pointers in the inode structure.
directBlocksCount :: Int
directBlocksCount = 12

otherExecute :: Inode -> Bool
otherExecute i = let mode = iMode i
                 in  (mode .&. inodeOtherExecute) == inodeOtherExecute

otherWrite :: Inode -> Bool
otherWrite i = let mode = iMode i
               in  (mode .&. inodeOtherWrite) == inodeOtherWrite

otherRead :: Inode -> Bool
otherRead i = let mode = iMode i
              in  (mode .&. inodeOtherRead) == inodeOtherRead

groupExecute :: Inode -> Bool
groupExecute i = let mode = iMode i
                 in  (mode .&. inodeGroupExecute) == inodeGroupExecute

groupWrite :: Inode -> Bool
groupWrite i = let mode = iMode i
               in  (mode .&. inodeGroupWrite) == inodeGroupWrite

groupRead :: Inode -> Bool
groupRead i = let mode = iMode i
              in  (mode .&. inodeGroupRead) == inodeGroupRead

ownerExecute :: Inode -> Bool
ownerExecute i = let mode = iMode i
                 in  (mode .&. inodeOwnerExecute) == inodeOwnerExecute

ownerWrite :: Inode -> Bool
ownerWrite i = let mode = iMode i
               in  (mode .&. inodeOwnerWrite) == inodeOwnerWrite

ownerRead :: Inode -> Bool
ownerRead i = let mode = iMode i
              in  (mode .&. inodeOwnerRead) == inodeOwnerRead

sticky :: Inode -> Bool
sticky i = let mode = iMode i
           in  (mode .&. inodeSticky) == inodeSticky

setGID :: Inode -> Bool
setGID i = let mode = iMode i
           in  (mode .&. inodeSetGID) == inodeSetGID

setUID :: Inode -> Bool
setUID i = let mode = iMode i
           in  (mode .&. inodeSetUID) == inodeSetUID

typeFifo :: Inode -> Bool
typeFifo i = let mode = iMode i
             in  (mode .&. inodeTypeFifo) == inodeTypeFifo

typeCharDevice :: Inode -> Bool
typeCharDevice i = let mode = iMode i
                   in  (mode .&. inodeTypeCharDevice) == inodeTypeCharDevice

typeDirectory :: Inode -> Bool
typeDirectory i = let mode = iMode i
                  in  (mode .&. inodeTypeDirectory) == inodeTypeDirectory

typeBlockDevice :: Inode -> Bool
typeBlockDevice i = let mode = iMode i
                    in  (mode .&. inodeTypeBlockDevice) == inodeTypeBlockDevice

typeFile :: Inode -> Bool
typeFile i = let mode = iMode i
             in  (mode .&. inodeTypeFile) == inodeTypeFile

typeSocket :: Inode -> Bool
typeSocket i = let mode = iMode i
               in  (mode .&. inodeTypeSocket) == inodeTypeSocket

typeLink  :: Inode -> Bool
typeLink i = let mode = iMode i
             in  (mode .&. inodeTypeLink) == inodeTypeLink

-- Indicates whether the node is executable for other users.
inodeOtherExecute :: Word16
inodeOtherExecute = 0x0001

-- Indicates whether the node is writeable for other users.
inodeOtherWrite :: Word16
inodeOtherWrite = 0x0002

-- Indicates whether the node is readable for other users.
inodeOtherRead :: Word16
inodeOtherRead = 0x0004

-- Indicates whether the node is executable for users in the owner's group.
inodeGroupExecute :: Word16
inodeGroupExecute = 0x0008

-- Indicates whether the node is writeable for users in the owner's group.
inodeGroupWrite :: Word16
inodeGroupWrite = 0x0010

-- Indicates whether the node is readable for users in the owner's group.
inodeGroupRead :: Word16
inodeGroupRead = 0x0020

-- Indicates whether the node is executable for the owner.
inodeOwnerExecute :: Word16
inodeOwnerExecute = 0x0040

-- Indicates whether the node is writeable for the owner.
inodeOwnerWrite :: Word16
inodeOwnerWrite = 0x0080

-- Indicates whether the node is readable for the owner.
inodeOwnerRead :: Word16
inodeOwnerRead = 0x0100

-- Indicates whether to allow only the owner of a file to modify it.
inodeSticky :: Word16
inodeSticky = 0x0200

-- Indicates whether to allow users to execute the file with group permissions.
inodeSetGID :: Word16
inodeSetGID = 0x0400

-- Indicates whether to allow users to execute the file with owner permissions.
inodeSetUID :: Word16
inodeSetUID = 0x0800

-- Indicates whether the node is a FIFO.
inodeTypeFifo :: Word16
inodeTypeFifo = 0x1000

-- Indicates whether the node is a character device.
inodeTypeCharDevice :: Word16
inodeTypeCharDevice = 0x2000

-- Indicates whether the node is a directory.
inodeTypeDirectory :: Word16
inodeTypeDirectory = 0x4000

-- Indicates whether the node is a block device.
inodeTypeBlockDevice :: Word16
inodeTypeBlockDevice = 0x6000

-- Indicates whether the node is a regular file.
inodeTypeFile :: Word16
inodeTypeFile = 0x8000

-- Indicates whether the node is a socket type inode.
inodeTypeSocket :: Word16
inodeTypeSocket = 0xC000

-- Indicates whether the node is a symbolic link.
inodeTypeLink :: Word16
inodeTypeLink = 0xA000
------------------------------------------------------------------------------------------------------------------------
-- INSTANCES
instance Show Inode where
    show ino = concat [ _type
                      , "  [", owner, group, other, "]"
                      , "  uid=", show $ iUid ino
                      , "  gid=", show $ iGid ino
                      , "  size=",show $ byteCount $ fromIntegral $ iSize ino
                      ]
        where _type
                | typeBlockDevice ino = "blk "
                | typeCharDevice  ino = "char"
                | typeDirectory   ino = "dir "
                | typeFifo        ino = "pipe"
                | typeFile        ino = "file"
                | typeLink        ino = "link"
                | typeSocket      ino = "sock"
                | otherwise           = "unknown (iMode=" ++ showIntAtBase (2 :: Int) intToDigit (fromIntegral $ iMode ino) ")"
              owner = f (ownerRead,ownerWrite,ownerExecute)
              group = f (groupRead,groupWrite,groupExecute)
              other = f (otherRead,otherWrite,otherExecute)
              f (r,w,x) = foldr (:) "" [ if r ino then 'r' else '-'
                                       , if w ino then 'w' else '-'
                                       , if x ino then 'x' else '-'
                                       ]
