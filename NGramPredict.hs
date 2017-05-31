module Main where

import Control.Monad (replicateM_)
-- import System.Environment (getArgs)
import System.IO

main :: IO [()]
main = do
    --args <- getArgs
    --mapM putStrLn args
    file <- openFile "text.txt" ReadMode
    prefix <- getPrefix file 2 2 12
    mapM putStrLn prefix


-- file -> length of prefix -> line -> column -> prefix
getPrefix :: Handle -> Int -> Int -> Int -> IO [String]
getPrefix file prefixLen line col = do
    replicateM_ (line - 1) $ hGetLine file
    foundLine <- hGetLine file
    return $ lastN prefixLen $ words $ fst $ splitAt col foundLine
    where
        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs
