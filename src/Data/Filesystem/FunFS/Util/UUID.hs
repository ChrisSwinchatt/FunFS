{- |
 - Module      : Data.Filesystem.FunFS.Util.UUID
 - Description : Generate Version 4 UUIDs in the IO monad.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.Util.UUID ( UUID
                                       , nextRandom
                                       , toWords
                                       , toBytes
                                       , toString
                                       , fromByteString
                                       ) where

import Data.Filesystem.FunFS.Util.Bits
import Data.UUID    (UUID,toWords,toString,fromByteString)
import Data.UUID.V4 (nextRandom)
import Data.Word

toBytes :: UUID -> [Word8]
toBytes u = let (x,y,z,w) = toWords u
            in  concatMap toBytes32 [x,y,z,w]
