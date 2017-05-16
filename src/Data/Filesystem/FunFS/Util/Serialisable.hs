{- |
 - Module      : Data.Filesystem.FunFS.Util.Serialisable
 - Description : 'Serialisable' typeclass.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.Util.Serialisable (Serialisable(..)) where

import Control.Monad   (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified Data.ByteString.Lazy as B

{- | Typeclass implemented by serialisable objects.
 - Minimum complete definition: 'get' and 'put'.
 -
 - Note: We cannot use the Data.Binary.Binary class here because it always uses network byte ordering (big endian),
 - while Ext2 always uses little endian.
 -}
class Serialisable a where
    -- | Retrieve a value of type 'a'.
    get :: Get a

    -- | Store a value of type 'a'.
    put :: a -> Put

    -- | Retrieve a list of 'a's.
    getList :: Int -> Get [a]
    getList n = replicateM n get

    -- | Store a list of 'a's.
    putList :: [a] -> Put
    putList = foldMap put

    -- | Decode a value of type 'a' from a lazy ByteString.
    decode :: B.ByteString -> a
    decode = runGet get

    -- | Encode a value of type 'a' into a lazy ByteString.
    encode :: a -> B.ByteString
    encode = runPut . put

    -- | Decode a list of 'a's from a lazy ByteString.
    decodeList :: Int -> B.ByteString -> [a]
    decodeList = runGet . getList

    -- | Encode a list of 'a's into a lazy ByteString.
    encodeList :: [a] -> B.ByteString
    encodeList = runPut . putList

instance Serialisable Word8 where
    get = getWord8
    put = putWord8

instance Serialisable Word16 where
    get = getWord16le
    put = putWord16le

instance Serialisable Word32 where
    get = getWord32le
    put = putWord32le

-- TESTS
prop_list :: [Word32] -> Bool
prop_list xs = xs == ys
    where n  = length     xs
          bs = encodeList xs
          ys = decodeList n bs
