{- |
 - Module      : Data.FileSystem.FunFS.Util.Time
 - Description : POSIX time.
 - Copyright   : (c) 2017-2019 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <chris@swinchatt.dev>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Util.Time (getTimeStamp, showTimeStamp) where
--
import Data.Time.Format
import Data.Time.Clock.POSIX

-- | Get a POSIX timestamp representing the current time as seconds since the Epoch.
getTimeStamp :: (Integral a) => IO a
getTimeStamp = fmap round getPOSIXTime

-- | Get a GMT timestamp string from a POSIX timestamp using the default locale.
showTimeStamp :: (Integral a) => a -> String
showTimeStamp = formatTime defaultTimeLocale "%c" . posixSecondsToUTCTime . fromIntegral
