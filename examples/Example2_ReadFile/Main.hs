{- |
 - Module      : Main
 - Description : Example2_ReadFile - List the contents of a directory.
 - Copyright   : (c) 2017-2018 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <c.swinchatt1@uni.brighton.ac.uk>
 - Stability   : experimental
 - Portability : portable
 -}
module Main where
--
import Data.FileSystem.FunFS (FS)
import Data.Maybe
import System.Environment

import qualified Data.ByteString.Lazy  as B
import qualified Data.FileSystem.FunFS as FS

example2 :: String -> FS B.ByteString
example2 path = do
    file  <- FS.fOpen path FS.ReadOnly
    size' <- FS.fGetSize file
    case size' of
        Just size -> do
            bytes <- FS.fRead file size
            FS.fClose file
            FS.unmount
            return bytes
        Nothing -> error "nothing"

main :: IO ()
main = do
    args <- getArgs
    if   length args < 2
    then do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " DEVICE PATH"
        putStrLn "Print the contents of the file named by PATH"
    else do
        let device = head args
            path   = args !! 1
        fileData <- FS.withMount device $ example2 path
        B.putStr fileData
