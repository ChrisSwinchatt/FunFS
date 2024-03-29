{- |
 - Module      : Data.FileSystem.FunFS.Util.Bits
 - Description : Bitwise operations on words.
 - Copyright   : (c) 2017-2019 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <chris@swinchatt.dev>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.Bits where

import Data.Bits
import Data.Word

toBytes16 :: Word16 -> [Word8]
toBytes16 x = map fromIntegral [ x .&. 0xFF
                               , (x .&. 0xFF00) `shiftR` 8
                               ]

toBytes32 :: Word32 -> [Word8]
toBytes32 x = map fromIntegral [  x .&. 0xFF
                               , (x .&. 0xFF00) `shiftR`  8
                               , (x .&. 0xFFF0) `shiftR` 16
                               , (x .&. 0xFFFF) `shiftR` 24
                               ]
