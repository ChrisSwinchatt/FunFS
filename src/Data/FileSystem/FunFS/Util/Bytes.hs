{- |
 - Module      : Data.FileSystem.FunFS.Util.Bytes
 - Description : SI binary prefixes for byte counts.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.Bytes where

-- | Byte count with SI binary prefix.
data Bytes = Bytes Int -- ^ Number of bytes.
           | KiB   Int -- ^ Number of kibibytes (1024 bytes).
           | MiB   Int -- ^ Number of mebibytes (1024 kiB).
           | GiB   Int -- ^ Number of gibibytes (1024 MiB).
           | TiB   Int -- ^ Number of tebibytes (1024 GiB).

-- | Get raw number of bytes.
fromBytes :: Bytes -> Int
fromBytes (Bytes x) = x
fromBytes (KiB   x) = 1024*x
fromBytes (MiB   x) = 1024*1024*x
fromBytes (GiB   x) = 1024*1024*1024*x
fromBytes (TiB   x) = 1024*1024*1024*1024*x

instance Eq Bytes where
    a == b = fromBytes a == fromBytes b

instance Ord Bytes where
    a <  b = fromBytes a < fromBytes b
    a <= b = a < b || a == b

instance Show Bytes where
    show (Bytes x) = show x ++   " B"
    show (KiB   x) = show x ++ " kiB"
    show (MiB   x) = show x ++ " MiB"
    show (GiB   x) = show x ++ " GiB"
    show (TiB   x) = show x ++ " TiB"

instance Num Bytes where
    a + b         = Bytes $ fromBytes a + fromBytes b
    a * b         = Bytes $ fromBytes a * fromBytes b
    abs           = Bytes . abs    . fromBytes
    signum        = Bytes . signum . fromBytes
    negate        = Bytes . negate . fromBytes
    fromInteger   = Bytes . fromInteger

byteCount :: Int -> Bytes
byteCount x
    | x < 1024                = Bytes x
    | x < 1024*1024           = KiB   $ x `div` 1024
    | x < 1024*1024*1024      = MiB   $ x `div` (1024*1024)
    | x < 1024*1024*1024*1024 = GiB   $ x `div` (1024*1024*1024)
    | otherwise               = TiB   $ x `div` (1024*1024*1024*1024)
