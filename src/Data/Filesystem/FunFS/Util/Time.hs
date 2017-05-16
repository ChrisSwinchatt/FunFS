{- |
 - Module      : Data.Filesystem.FunFS.Util.Time
 - Description : POSIX time.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.Filesystem.FunFS.Util.Time (getTimeStamp, showTimeStamp) where
--
import Data.Time.Format
import Data.Time.Clock.POSIX

-- | Get a POSIX timestamp representing the current time as seconds since the Epoch.
getTimeStamp :: (Integral a) => IO a
getTimeStamp = fmap round getPOSIXTime

-- | Get a GMT timestamp string from a POSIX timestamp using the default locale.
showTimeStamp :: (Integral a) => a -> String
showTimeStamp = formatTime defaultTimeLocale "%c" . posixSecondsToUTCTime . fromIntegral
