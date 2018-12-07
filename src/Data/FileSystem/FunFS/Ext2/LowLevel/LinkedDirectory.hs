{- |
 - Module      : Data.FileSystem.FunFS.Ext2.LowLevel.LinkedDirectory
 - Description : Linked directory
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
{-# LANGUAGE TemplateHaskell #-}
module Data.FileSystem.FunFS.Ext2.LowLevel.LinkedDirectory where

import Data.FileSystem.FunFS.Ext2.LowLevel.Struct
import Data.Word

-- Linked directory.
struct "LinkedDirectory"
       [ field "dInode"   [t|Word32|]     -- inode    (4 bytes)
       , field "dRecLen"  [t|Word16|]     -- rec_len  (2 bytes)
       , field "dNameLen" [t|Word16|]     -- name_len (2 bytes)
       , array "dName"    [t|Word8|]  255 -- name     (255 bytes)
       ]
       ["Eq","Show"]

linkedDirectorySize :: (Integral a) => a
linkedDirectorySize = sizeof emptyLinkedDirectory

emptyLinkedDirectory :: LinkedDirectory
emptyLinkedDirectory = LinkedDirectory { dInode   = 0
                                       , dRecLen  = 0
                                       , dNameLen = 0
                                       , dName    = replicate 255 0
                                       }
