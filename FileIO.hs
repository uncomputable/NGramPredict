module FileIO where

import Control.Monad (replicateM_)
import Data.List.Split (splitOn)
import System.IO

data ModelHeader = Header Int [Int] deriving Show
data NGram = NGram Double [String] Double deriving Show
type NGrams = [[NGram]]
data Model = Model ModelHeader NGrams deriving Show


emptyModel :: Model
emptyModel = Model (Header 0 []) []


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


-- | Reads the header of an ARPA file.
-- CAUTION: The handle MUST be at the beginning of the file or this function
-- won't work!
readHeader ::
    Handle -> -- ^ handle of model file
    IO Header -- ^ read model header
readHeader modelHandle = go $ Header 0 []
    where
        go :: ModelHeader -> IO ModelHeader
        go header@(Header n nums) = do
            line <- maybeGetLine modelHandle
            let parsed = maybeSplit "=" line

            case parsed of
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
        go :: [NGram] -> IO [NGram]
        go ngrams = do
            line <- maybeGetLine modelHandle
            let parsed = maybeSplit "\t" line

            case parsed of
                Nothing -> return ngrams
                Just [""] -> return ngrams
                Just [_] -> go ngrams
                Just (pStr : xs) -> let p = read pStr
                                        w = init xs
                                        b = read $ last xs
                                    in go $ (NGram p w b) : ngrams
                _ -> undefined
