-- | Contains the main function and handles command line input.
module Main (main) where

import FileIO
import Probability
import System.Environment (getArgs, getProgName)

-- | Entry point of the application.
-- Console arguments: number, model, file, line, column
main :: IO ()
main = do
    args <- getArgs
    if length args < 5
    then printHelp
    else do
        let number = read $ args !! 0
        let modelPath = args !! 1
        let textPath = args !! 2
        let line = read $ args !! 3
        let colomn = read $ args !! 4

        model <- readModel modelPath
        prefix <- getPrefix textPath model line colomn
        let prediction = predict number prefix model

        putStrLn "Prefix:"
        putStrLn $ unwords prefix
        putStrLn "\nPredictions:"
        mapM_ putStrLn prediction


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
