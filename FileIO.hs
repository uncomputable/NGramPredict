-- | Handles all I/O operations like reading from files.
module FileIO (getPrefix, readModel) where

import Model
import Control.Monad.State.Lazy
import qualified Data.Bimap as Bimap
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
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
            case go content of
               Left err -> error err
               Right prefix -> return prefix

        go :: String -> Either String [String]
        go content = do
            when (line <= 0 || col <= 0) $
                Left $ "<line> and <column> must be greater than zero. "
                ++ "Please choose larger values."

            let textLines      = lines content
                maybeFoundLine = textLines ?!! (line - 1)
                maxLen         = headerNMax (modelHeader model) - 1

            when (isNothing maybeFoundLine) $
                Left $ "<line> is larger than the line count of the file! "
                ++ "Please choose a smaller value."

            let foundLine  = fromJust maybeFoundLine
                lineFront' = words $ fst $ splitAt col foundLine
                lineFront  = if isSpace $ foundLine !! (col - 1)
                             then lineFront'
                             else init lineFront'

            when (length foundLine < col) $
                Left $ "The line in the text file is shorter than <column>! "
                ++ "Maybe <column> is too large or <line> contains an error."

            return $ lastN maxLen lineFront

        (?!!) :: [a] -> Int -> Maybe a
        (?!!) [] _ = Nothing
        (?!!) (x : xs) n
            | n == 0 = Just x
            | otherwise = (?!!) xs (n - 1)

        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs


-- | Reads the entire model from an ARPA file.
readModel
    :: String    -- ^ path to model file
    -> IO Model  -- ^ read model
readModel modelPath = try `catchIOError` handler
    where
        try :: IO Model
        try = do
            content <- readFile modelPath
            let ls     = lines content
                header = readHeader ls
                (allNGrams, mapping) = readAllNGrams ls $ headerNMax header
            return $ Model header allNGrams mapping


-- | Handler for exceptions that can occur during I/O actions like opening
-- a file.
handler
    :: IOError  -- ^ error that occurred
    -> IO a     -- ^ I/O action of the handler
handler e
    | isDoesNotExistError e = error $ "File "
        ++ maybe "" (\s -> '\'' : s ++ "\'") (ioeGetFileName e)
        ++ " could not be found! Maybe the path contains an error."
    | otherwise = ioError e


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
                [""]     -> header
                [_, num] -> let header' = Header (n + 1) (nums ++ [read num])
                            in go rest header'
                _        -> go rest header


-- | Reads the n-gram sections of an ARPA file that follow the header.
readAllNGrams
    :: [String]            -- ^ lines of model file
    -> Int                 -- ^ maximum n for n-grams
    -> ([NGrams], UniMap)  -- ^ all read n-grams for n = 1..max
                           --   and unigram mapping
readAllNGrams ls nMax =
    let ls' = drop (nMax + 2) ls
        (allNGrams, finalBuilder) = runState (go ls' 1 Map.empty)
            Builder {nextInt = 0, currMap = Bimap.empty}
    in (allNGrams, currMap finalBuilder)
    where
        go :: [String] -> Int -> NGrams -> State MapBuilder [NGrams]
        go [] _ ngrams = return [ngrams]
        go (line : rest) n ngrams = do
            let ws = words line
            case ws of
                []  -> do otherNGrams <- go rest (n + 1) Map.empty
                          otherNGrams `seq` return $ ngrams : otherNGrams
                [_] -> go rest n ngrams
                _   -> do (ngram, prob) <- parseNGram ws $ n == nMax
                          let ngrams' = Map.insert ngram prob ngrams
                          ngrams' `seq` go rest n ngrams'

        parseNGram :: [String] -> Bool -> State MapBuilder ([Int], Prob)
        parseNGram [] _ = undefined
        parseNGram (pStr : xs) nMaximal = do
            let p = read pStr
                w = if nMaximal then xs else init xs
                b = if nMaximal then Nothing else Just $ read $ last xs
            ngram <- lookupInsert w
            ngram `seq` p `seq` b `seq` return (ngram, Prob p b)

        lookupInsert :: [String] -> State MapBuilder [Int]
        lookupInsert [] = return []
        lookupInsert (w : ws) = do
            int <- lookupInsert' w
            otherInts <- lookupInsert ws
            int `seq` return $ int : otherInts

        lookupInsert' :: String -> State MapBuilder Int
        lookupInsert' w = do
            Builder {nextInt = next, currMap = mapping} <- get
            let maybeVal = Bimap.lookup w mapping
                mapping' = if isNothing maybeVal
                           then Bimap.insert w next mapping
                           else mapping
                next'    = if isNothing maybeVal
                           then next + 1
                           else next
            put Builder {nextInt = next', currMap = mapping'}
            return $ fromMaybe next maybeVal
