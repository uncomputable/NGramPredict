module Main where

import FileIO
import System.Environment (getArgs, getProgName)
import System.IO

-- | Entry point of the application.
-- Console arguments: number, model, file, line, column
main :: IO ()
main = do
    args <- getArgs
    if length args < 5
    then printHelp
    else do
        let modelName = args !! 1
        let fileName = args !! 2
        let line = read $ args !! 3
        let colomn = read $ args !! 4

        textHandle <- openFile fileName ReadMode
        prefix <- getPrefix textHandle 2 line colomn
        mapM_ putStrLn prefix
        hClose textHandle

        modelHandle <- openFile modelName ReadMode
        header <- readHeader modelHandle
        putStrLn $ show header

        oneGrams <- readNGrams modelHandle
        putStrLn $ show oneGrams
        hClose modelHandle


-- | Prints usage advice on the command line.
printHelp :: IO ()
printHelp = do
    progName <- getProgName
    mapM_ putStrLn
        [ progName ++ " <number> <model> <file> <line> <column>"
        , "  where"
        , "    <number> ... number of word suggestions"
        , "    <model>  ... path to ARPA file with n-gram model"
        , "    <file>   ... path to text file that is to be analysed"
        , "    <line>   ... line number for suggestion position (1-based)"
        , "    <column> ... column number for suggestion position (1-based)"
        ]
