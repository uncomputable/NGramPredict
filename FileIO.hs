-- | Handles all I/O operations like reading from files.
module FileIO (getPrefix, readModel) where

import Model
import Control.Monad (replicateM_)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import System.IO
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
            textHandle <- openFile textPath ReadMode
            replicateM_ (line - 1) $ hGetLine textHandle
            foundLine <- hGetLine textHandle
            hClose textHandle
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
            modelHandle <- openFile modelPath ReadMode
            header <- readHeader modelHandle
            allNGrams <- readAllNGrams modelHandle $ headerGetNMax header
            hClose modelHandle
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
-- CAUTION: The handle MUST be at the beginning of the file or this function
-- won't work!
readHeader
    :: Handle     -- ^ handle of model file
    -> IO Header  -- ^ read model header
readHeader modelHandle = go $ Header 0 []
    where
        go :: Header -> IO Header
        go header@(Header n nums) = do
            line <- maybeGetLine modelHandle
            let split = maybeSplit "=" line

            case split of
                Nothing -> return header
                Just [""] -> return header
                Just [_, num] -> let header' = Header (n + 1) (nums ++ [read num])
                                 in go header'             --  ^^^^^^^^^^^^^^^^^^ performance goes over board
                _ -> go header

        maybeSplit :: String -> Maybe String -> Maybe [String]
        maybeSplit _ Nothing = Nothing
        maybeSplit del (Just s) = Just $ splitOn del s


-- | Reads the n-gram sections of an ARPA file that follow the header.
-- CAUTION: The handle MUST be hehind the ARPA header or this function
-- won't work!
readAllNGrams
    :: Handle       -- ^ handle of model file
    -> Int          -- ^ maximum n for n-grams
    -> IO [NGrams]  -- ^ all read n-grams for n = 1..max
readAllNGrams modelHandle nMax = go 1
    where
        go :: Int -> IO [NGrams]
        go n
            | n == nMax = do
                ngrams <- readNGrams Map.empty True
                ngrams `seq` return [ngrams]
            | otherwise = do
                ngrams <- readNGrams Map.empty False
                rest <- go (n + 1)
            --  ^^^^^^^^^^^^^^^^^^ performance ???
                ngrams `seq` return $ ngrams : rest

        readNGrams :: NGrams -> Bool -> IO NGrams
        readNGrams ngrams isMax = do
            line <- maybeGetLine modelHandle
            let split = maybeWords line

            case split of
                Nothing -> return ngrams
                Just [] -> return ngrams
                Just [_] -> readNGrams ngrams isMax
                Just split' -> let (ngram, prob) = if isMax
                                                   then parseMaxNGram split'
                                                   else parseNGram split'
                                   ngrams' = Map.insert ngram prob ngrams
                               in ngrams' `seq` readNGrams ngrams' isMax
                              --  ^^^^^^^^^^^^^ 0.5x memory, 10x runtime

        maybeWords :: Maybe String -> Maybe [String]
        maybeWords Nothing = Nothing
        maybeWords (Just s) = let split = words s in split `seq` Just split

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


-- | Wrapper for hGetLine that handles EOF using the Maybe monad.
maybeGetLine
    :: Handle             -- ^ handle of file that is to be read from
    -> IO (Maybe String)  -- ^ Just (read string) or Nothing
maybeGetLine handle = do
    eof <- hIsEOF handle
    if eof
    then return Nothing
    else do
        line <- hGetLine handle
        return $ Just line
