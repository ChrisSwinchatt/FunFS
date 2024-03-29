{- |
 - Module      : Data.FileSystem.FunFS.Util.UUID
 - Description : Generate Version 4 UUIDs in the IO monad.
 - Copyright   : (c) 2017-2019 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <chris@swinchatt.dev>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.UUID ( UUID
                                       , nextRandom
                                       , toWords
                                       , toBytes
                                       , toString
                                       , fromByteString
                                       ) where

import Data.FileSystem.FunFS.Util.Bits
import Data.UUID    (UUID,toWords,toString,fromByteString)
import Data.UUID.V4 (nextRandom)
import Data.Word

toBytes :: UUID -> [Word8]
toBytes u = let (x,y,z,w) = toWords u
            in  concatMap toBytes32 [x,y,z,w]
