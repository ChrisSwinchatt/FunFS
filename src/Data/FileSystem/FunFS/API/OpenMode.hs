{- |
 - Module      : Data.FileSystem.FunFS.API.OpenMode
 - Description : File open mode..
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.OpenMode (OpenMode(..)) where

-- | File open mode.
data OpenMode = ReadOnly   -- ^ Allow reading only.
              | WriteOnly  -- ^ Allow writing only.
              | ReadWrite  -- ^ Allow reading and writing.
              | Append     -- ^ Allow writing at the end of the file only.
              deriving (Enum,Eq,Ord)

instance Show OpenMode where
    show ReadOnly  = "read-only"
    show WriteOnly = "write-only"
    show ReadWrite = "read-write"
