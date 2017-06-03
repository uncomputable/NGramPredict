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


maybeGetLine ::
    Handle ->
    IO (Maybe String)
maybeGetLine handle = do
    eof <- hIsEOF handle
    if eof
    then return Nothing
    else do
        line <- hGetLine handle
        return $ Just line


maybeSplit ::
    String ->
    Maybe String ->
    Maybe [String]
maybeSplit _ Nothing = Nothing
maybeSplit dels (Just s) = Just $ splitOn dels s


readHeader ::
    Handle ->      -- ^ handle of model file
    IO ModelHeader -- ^ read model header
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


readNGrams ::
    Handle ->  -- ^ handle of model file
    IO [NGram] -- ^ read n-grams
readNGrams modelHandle = go []
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
