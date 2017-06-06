-- | Contains functions for computing probabilities using a language model.
module Probability (computeProb) where

import Model
import qualified Data.Map.Strict as Map
import Data.Maybe

-- | Wrapper for computeProb' that takes full models instead of a list of
-- maps of n-grams.
computeProb
    :: String    -- ^ word w_i following prefix
    -> [String]  -- ^ prefix p
    -> Model     -- ^ language model with n-grams
    -> Double    -- ^ probability p(w_i | p)
computeProb w_i prefix model = computeProb' w_i prefix $ extractAllNGrams model


-- | Computes the probablity of a word following a specific prefix.
computeProb'
    :: String    -- ^ word w_i following prefix
    -> [String]  -- ^ prefix p
    -> [NGrams]  -- ^ list of maps of n-grams
    -> Double    -- ^ probability (w_i | p)
computeProb' w_i fullPrefix allNGrams = (10 **) $ go $ fullPrefix ++ [w_i]
    where
        go :: [String] -> Double
        go ngram = let prob = getProb ngram
                   in maybe (backoff ngram) extractProb prob

        backoff :: [String] -> Double
        backoff [] = undefined -- unigram w_i was not in model at all!
        backoff (_ : shorter) = let prob = getProb shorter
                                    b = maybe 0 extractBackoff prob
                                in b + go shorter

        firstJust :: [Maybe a] -> Maybe a
        firstJust [] = Nothing
        firstJust (x : xs)
            | isNothing x = firstJust xs
            | otherwise = x

        getProb :: [String] -> Maybe Prob
        getProb ngram = let probs = map (Map.lookup ngram) allNGrams
                        in firstJust probs
                        -- thank you, lazyness!
