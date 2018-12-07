{- |
 - Module      : Main
 - Description : Example0_Mount - Mount a filesystem and print statistics about it on the screen.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Main where
--
import Data.FileSystem.FunFS (FS)
import System.Environment

import qualified Data.FileSystem.FunFS as FS

example0 :: FS String
example0 = do
    stats <- FS.getStatistics
    FS.unmount
    return stats

main :: IO ()
main = do
    args <- getArgs
    if   null args
    then do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " DEVICE\nMount DEVICE and print statistics about its filesystem"
    else do
        stats <- FS.withMount (head args) example0
        putStrLn stats
