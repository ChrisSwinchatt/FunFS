{- |
 - Module      : Main
 - Description : Example1_ReadDir - List the contents of a directory.
 - Copyright   : (c) 2017 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Main where
--
import Data.Filesystem.FunFS (FS)
import System.Environment

import qualified Data.Filesystem.FunFS as FS

example1 :: String -> FS [String]
example1 path = do
    dir     <- FS.openDir path FS.ReadOnly
    entries <- FS.readDirToEnd dir
    FS.closeDir dir
    FS.unmount
    return $ map show entries

main :: IO ()
main = do
    args <- getArgs
    if   length args < 2
    then do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " DEVICE PATH\nList the contents of the directory named by PATH"
    else do
        let device = head args
            path   = args !! 1
        entries <- FS.withMount device $ example1 path
        putStr $ concat [ show $ length entries, " entries in directory ", path, "\n", unlines entries ]
