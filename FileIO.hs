-- | Handles all I/O operations like reading from files.
module FileIO (getPrefix, readModel) where

import Model
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import System.IO.Error

-- | Reads a prefix from a text file and returns it. The maximum length of
-- said prefix is determined by the n-grams of maximum length of the language
-- model.
getPrefix
    :: String       -- ^ path to text file
    -> Model        -- ^ language model
    -> Int          -- ^ line number (1-based)
    -> Int          -- ^ column number (1-based)
    -> IO [String]  -- ^ found prefix
getPrefix textPath model line col = try `catchIOError` handler
    where
        try :: IO [String]
        try = do
            content <- readFile textPath
            let foundLine = ((!! (line - 1)) . lines) content
            -- too small or high <line> must still be caught
            let maxLen = headerGetNMax (extractHeader model) - 1
            let lineFront = words $ fst $ splitAt col foundLine
            _ <- detectErrors foundLine lineFront
            return $ lastN maxLen lineFront

        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs

        detectErrors :: String -> [String] -> IO [String]
        detectErrors foundLine lineFront
            | length foundLine < col = error $ "The line in the text file was "
                ++ "shorter than <column>! Maybe <column> is too large or "
                ++ "<line> contains an error."
            | invalidDelimiter $ foundLine !! (col - 1) = error
                $ "Failed to extract the prefix, because <column> is "
                ++ "pointing to a non-whitespace character!"
            | length lineFront == 0 = error
                $ "There isn't a single word in front of <column>! Maybe "
                ++ "<column> is too small or <line> contains an error."
            | otherwise = return []

        invalidDelimiter :: Char -> Bool
        invalidDelimiter c
            | c == ' ' || c == '\t' = False
            | otherwise = True


-- | Reads the entire model from an ARPA file.
readModel
    :: String    -- ^ path to model file
    -> IO Model  -- ^ read model
readModel modelPath = try `catchIOError` handler
    where
        try :: IO Model
        try = do
            content <- readFile modelPath
            let ls = lines content
            let header = readHeader ls
            let allNGrams = readAllNGrams ls $ headerGetNMax header
            return $ Model header allNGrams


-- | Handler for exceptions that can occur during I/O actions like opening
-- a file.
handler
    :: IOError  -- ^ error that occurred
    -> IO a     -- ^ I/O action of the handler
handler e
    | isDoesNotExistError e = error $ "File "
        ++ fileToString
        ++ " could not be found! Maybe the path contains an error."
    | isEOFError e = error $ "Went past the last line of the file "
        ++ fileToString
        ++ "! Maybe the <line> parameter contains an error."
    | otherwise = ioError e
    where
        fileToString :: String
        fileToString = maybe "" (\s -> '\'' : s ++ "\'") $ ioeGetFileName e


-- | Reads the header of an ARPA file.
readHeader
    :: [String]  -- ^ lines of model file
    -> Header    -- ^ read model header
readHeader ls = go ls $ Header 0 []
    where
        go :: [String] -> Header -> Header
        go [] header = header
        go (line : rest) header@(Header n nums) =
            let split = splitOn "=" line
            in case split of
                [""] -> header
                [_, num] -> let header' = Header (n + 1) (nums ++ [read num])
                            in go rest header'
                _ -> go rest header


-- | Reads the n-gram sections of an ARPA file that follow the header.
readAllNGrams
    :: [String]  -- ^ lines of model file
    -> Int       -- ^ maximum n for n-grams
    -> [NGrams]  -- ^ all read n-grams for n = 1..max
readAllNGrams ls nMax = let ls' = drop (nMax + 2) ls
                        in go ls' 1 Map.empty
    where
        go :: [String] -> Int -> NGrams -> [NGrams]
        go [] _ ngrams = [ngrams]
        go (line : rest) n ngrams =
            let ws = words line
            in case ws of
                [] -> let otherNGrams = go rest (n + 1) Map.empty
                      in ngrams : otherNGrams
                [_] -> go rest n ngrams
                _ -> let (ngram, prob) = if n == nMax
                                         then parseMaxNGram ws
                                         else parseNGram ws
                         ngrams' = Map.insert ngram prob ngrams
                     in ngrams' `seq` go rest n ngrams'
                    --  ^^^^^^^^^^^^^ 0.5x memory

        parseNGram :: [String] -> ([String], Prob)
        parseNGram (pStr : xs) = let p = read pStr
                                     w = init xs
                                     b = read $ last xs
                                 in p `seq` w `seq` b `seq` (w, Prob p b)
        parseNGram _ = undefined

        parseMaxNGram :: [String] -> ([String], Prob)
        parseMaxNGram (pStr : xs) = let p = read pStr
                                        w = xs
                                    in p `seq` w `seq` (w, ProbMax p)
        parseMaxNGram _ = undefined
