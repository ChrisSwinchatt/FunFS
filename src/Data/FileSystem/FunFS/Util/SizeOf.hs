{- |
 - Module      : Data.FileSystem.FunFS.Util.SizeOf
 - Description : Compute sizes in bytes for types.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.SizeOf where

import Data.Int
import Data.Word

-- | Typeclass for computing the size of a type in bytes.
-- The Foreign.Storable class provides this but is larger.
class SizeOf a where
    sizeof :: (Integral b) => a -> b

-- INSTANCES
instance SizeOf Int8 where
    sizeof _ = 1

instance SizeOf Int16 where
    sizeof _ = 2

instance SizeOf Int32 where
    sizeof _ = 4

instance SizeOf Int64 where
    sizeof _ = 8

instance SizeOf Word8 where
    sizeof _ = 1

instance SizeOf Word16 where
    sizeof _ = 2

instance SizeOf Word32 where
    sizeof _ = 4

instance SizeOf Word64 where
    sizeof _ = 8
