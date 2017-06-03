module FileIO where

import Control.Monad (replicateM_)
import Data.List.Split (splitOn)
import System.IO

data Header = Header Int [Int] deriving Show
data NGram = NGram Double [String] Double | MaxNGram Double [String] deriving Show
type NGrams = [[NGram]]
data Model = Model Header NGrams deriving Show

-- | Returns the prefix for the word suggestion.
getPrefix ::
    Handle ->   -- ^ handle of text file
    Int ->      -- ^ length of prefix
    Int ->      -- ^ line number (1-based)
    Int ->      -- ^ column number (1-based)
    IO [String] -- ^ found prefix
getPrefix textHandle prefixLen line col = do
    replicateM_ (line - 1) $ hGetLine textHandle
    foundLine <- hGetLine textHandle
    return $ lastN prefixLen $ words $ fst $ splitAt col foundLine
    where
        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs


-- | Wrapper for hGetLine that handles EOF using the Maybe monad.
maybeGetLine ::
    Handle ->         -- ^ handle of file that is to be read from
    IO (Maybe String) -- ^ Just (read string) or Nothing
maybeGetLine handle = do
    eof <- hIsEOF handle
    if eof
    then return Nothing
    else do
        line <- hGetLine handle
        return $ Just line


-- | Wrapper for splitOn that works with the Maybe monad.
maybeSplit ::
    String ->       -- ^ delimiters for splitting
    Maybe String -> -- ^ Just (string that is to be split) or Nothing
    Maybe [String]  -- ^ Just (split string) or Nothing
maybeSplit _ Nothing = Nothing
maybeSplit dels (Just s) = Just $ splitOn dels s


-- | Reads the entire model from the ARPA file.
readModel ::
    String -> -- ^ path to model file
    IO Model  -- ^ read model
readModel modelPath = do
    modelHandle <- openFile modelPath ReadMode
    header <- readHeader modelHandle
    allNGrams <- readAllNGrams modelHandle $ headerGetNMax header
    hClose modelHandle
    return $ Model header allNGrams
    where
        headerGetNMax :: Header -> Int
        headerGetNMax (Header nMax _) = nMax


-- | Reads the header of an ARPA file.
-- CAUTION: The handle MUST be at the beginning of the file or this function
-- won't work!
readHeader ::
    Handle -> -- ^ handle of model file
    IO Header -- ^ read model header
readHeader modelHandle = go $ Header 0 []
    where
        go :: Header -> IO Header
        go header@(Header n nums) = do
            line <- maybeGetLine modelHandle
            let split = maybeSplit "=" line

            case split of
                Nothing -> return header
                Just [""] -> return header
                Just [_, num] -> let header' = Header (n+1) (nums ++ [read num])
                                 in go header'            -- ^^^^^^^^^^^^^^^^^^ performance goes over board
                _ -> go header


-- | Reads the n-gram sections of an ARPA file that follow the header.
-- CAUTION: The handle MUST be hehind the ARPA header or this function
-- won't work!
readAllNGrams ::
    Handle -> -- ^ handle of model file
    Int ->    -- ^ maximum n for n-grams
    IO NGrams -- ^ all read n-grams for n = 1..max
readAllNGrams modelHandle nMax = go 1
    where
        go :: Int -> IO NGrams
        go n
            | n == nMax = do
                ngrams <- readNGrams [] True
                return [ngrams]
            | otherwise = do
                ngrams <- readNGrams [] False
                rest <- go (n + 1)
                return $ ngrams : rest
             -- ^^^^^^^^^^^^^^^^^^ performance ???

        readNGrams :: [NGram] -> Bool -> IO [NGram]
        readNGrams ngrams isMax = do
            line <- maybeGetLine modelHandle
            let split = maybeSplit "\t" line

            case split of
                Nothing -> return ngrams
                Just [""] -> return ngrams
                Just [_] -> readNGrams ngrams isMax
                Just [] -> undefined
                Just split' -> let ngram = if isMax
                                           then parseMaxNGram split'
                                           else parseNGram split'
                               in readNGrams (ngram : ngrams) isMax

        parseNGram :: [String] -> NGram
        parseNGram (pStr : xs) = let p = read pStr
                                     w = init xs
                                     b = read $ last xs
                                 in NGram p w b
        parseNGram _ = undefined

        parseMaxNGram :: [String] -> NGram
        parseMaxNGram (pStr : xs) = let p = read pStr
                                        w = xs
                                    in MaxNGram p w
        parseMaxNGram _ = undefined
