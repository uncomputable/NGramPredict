-- | Contains functions for computing probabilities using a language model.
module Probability (predict) where

import Model
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Maybe (isNothing)

-- | Returns the n most probable unigrams following a given prefix and using
-- a specific language model.
predict
    :: Int       -- ^ number of predictions n
    -> [String]  -- ^ prefix p
    -> Model     -- ^ language model
    -> [String]  -- ^ list of n unigrams u with highest p(u | p)
predict n prefix model =
    let allNGrams = extractAllNGrams model
        oneGrams = head allNGrams
        wordsWithProbs = map (\[u] -> (u, computeProb u prefix allNGrams)) $ Map.keys oneGrams
                           --  ^^^ non-exhaustive pattern matching?
    in map fst $ foldl' selectBestN [] wordsWithProbs
    where
        selectBestN :: Ord b => [(a, b)] -> (a, b) -> [(a, b)]
        selectBestN best curr
            | length best < n = curr : best
            | otherwise = replaceSmaller curr best

        replaceSmaller :: Ord b => (a, b) -> [(a, b)] -> [(a, b)]
        replaceSmaller _ [] = []
        replaceSmaller x@(_, p1) (el@(_, p2) : rest)
            | p1 <= p2 = el : replaceSmaller x rest
            | otherwise = x : rest


-- | Computes the probablity of a word following a specific prefix, using a
-- specific language model.
computeProb
    :: String    -- ^ word w_i following prefix
    -> [String]  -- ^ prefix p
    -> [NGrams]  -- ^ list of maps of n-grams
    -> Double    -- ^ probability (w_i | p)
computeProb w_i fullPrefix allNGrams = (10 **) $ go $ fullPrefix ++ [w_i]
    where
        go :: [String] -> Double
        go ngram = let prob = getProb ngram
                   in maybe (backoff ngram) extractProb prob

        backoff :: [String] -> Double
        backoff [] = undefined -- unigram w_i was not in model at all! FIXME: needs error message
        backoff (_ : shorter) = let prob = getProb shorter
                                    b = maybe 0 extractBackoff prob
                                in b + go shorter

        getProb :: [String] -> Maybe Prob
        getProb ngram = let probs = map (Map.lookup ngram) allNGrams
                        in firstJust probs
                        -- thank you, lazyness!

        firstJust :: [Maybe a] -> Maybe a
        firstJust [] = Nothing
        firstJust (x : xs)
            | isNothing x = firstJust xs
            | otherwise = x
