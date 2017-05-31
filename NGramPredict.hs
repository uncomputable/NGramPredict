module Main where

import Control.Monad (replicateM_)
-- import System.Environment (getArgs)
import System.IO

-- | Entry point of the application.
-- Console arguments: number, model, file, line, column
main :: IO [()]
main = do
    --args <- getArgs
    --mapM putStrLn args
    file <- openFile "text.txt" ReadMode
    prefix <- getPrefix file 2 2 12
    mapM putStrLn prefix


-- | Returns the prefix for the word suggestion.
getPrefix ::
    Handle ->   -- ^ handle of text file
    Int ->      -- ^ length of prefix
    Int ->      -- ^ line number (1-based)
    Int ->      -- ^ column number (1-based)
    IO [String] -- ^ found prefix
getPrefix file prefixLen line col = do
    replicateM_ (line - 1) $ hGetLine file
    foundLine <- hGetLine file
    return $ lastN prefixLen $ words $ fst $ splitAt col foundLine
    where
        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs
