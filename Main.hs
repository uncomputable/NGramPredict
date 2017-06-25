-- | Contains the main function and handles command line input.
module Main (main) where

import FileIO
import Probability
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

-- | Entry point of the application.
-- Console arguments: number, model, file, line, column
main :: IO ()
main = do
    args <- getArgs
    if length args < 5
    then printHelp
    else do
        let maybeNumber = readMaybe $ args !! 0
            modelPath   = args !! 1
            textPath    = args !! 2
            maybeLine   = readMaybe $ args !! 3
            maybeColumn = readMaybe $ args !! 4

        let number = fromMaybe (error "Failed to parse the integer <number>!")
                maybeNumber
            line   = fromMaybe (error "Failed to parse the integer <line>!")
                maybeLine
            column = fromMaybe (error "Failed to parse the integer <column>!")
                maybeColumn

        model <- readModel modelPath
        prefix <- getPrefix textPath model line column
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
