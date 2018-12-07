{- |
 - Module      : Data.FileSystem.FunFS.API.Seek
 - Description : Primitive for computing the results of seek operations.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.API.Seek ( Seek(..)
                                      , isBegin
                                      , isEnd
                                      , isCurPos
                                      , computeSeek
                                      ) where
--
-- | Seek type.
data Seek = Begin  -- ^ Relative to file beginning.
          | End    -- ^ Relative to file end.
          | CurPos -- ^ Relative to current position.
          deriving (Enum,Eq,Ord)


-- | Indicates whether to seek from the beginning.
isBegin :: Seek -> Bool
isBegin Begin = True
isBegin _     = False

-- | Indicates whether to seek from the end.
isEnd :: Seek -> Bool
isEnd End = True
isEnd _   = False

-- | Indicates whether to seek from the current position.
isCurPos :: Seek -> Bool
isCurPos CurPos = True
isCurPos _      = False

-- | Computes a new seek position bounded by the minimum, current and maximum values.
computeSeek :: Integral a
           => Seek    -- ^ The seek type
           -> (a,a,a) -- ^ The seek boundaries in form (minimum,current,maximum)
           -> a       -- ^ The proposed seek offset. For example 1 if seeking by 1 byte.
           -> a       -- ^ The absolute position to seek to.
computeSeek sp (beg,cur,end) off
    | isBegin  sp = min (beg + off) end
    | isCurPos sp = min (cur + off) end
    | isEnd    sp = max (end - off) beg
