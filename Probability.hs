-- | Contains functions for computing probabilities using a language model.
module Probability (predict) where

import Model
import qualified Data.Bimap as Bimap
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)

-- | Returns the n most probable unigrams following a given prefix and using
-- a specific language model.
predict
    :: Int       -- ^ number of predictions n
    -> [String]  -- ^ prefix p
    -> Model     -- ^ language model
    -> [String]  -- ^ list of n unigrams u with highest p(u | p)
predict n prefix model =
    let mapping = modelUniMap model
        encPrefix = map (fromJust . (`Bimap.lookup` mapping)) prefix
        encPrediction = go encPrefix
    in map (fromJust . (`Bimap.lookupR` mapping)) encPrediction
    where
        go :: [Integer] -> [Integer]
        go encPrefix =
            let allNGrams = modelNGrams model
                uniGrams = Map.keys $ head allNGrams
                uniProbs =
                    map (\[u] -> (u, computeProb u encPrefix allNGrams)) uniGrams
                      --  ^^^ non-exhaustive pattern matching?
                sortedUniProbs =
                    sortBy (\x y -> compare (snd y) (snd x)) uniProbs
            in map fst $ take n sortedUniProbs


-- | Computes the probablity of a word following a specific prefix, using a
-- specific language model.
computeProb
    :: Integer    -- ^ word w_i following prefix (encoded)
    -> [Integer]  -- ^ prefix p (encoded)
    -> [NGrams]   -- ^ list of maps of n-grams
    -> Double     -- ^ probability (w_i | p)
computeProb w_i fullPrefix allNGrams = (10 **) $ go $ fullPrefix ++ [w_i]
    where
        go :: [Integer] -> Double
        go ngram = let maybeProb = getProb ngram
                   in maybe (goBackoff ngram) problty maybeProb

        goBackoff :: [Integer] -> Double
        goBackoff [] = undefined -- unigram w_i was not in model at all!
        goBackoff (_ : shorter) = let maybeProb = getProb shorter
                                      weight = maybe 0 backoff maybeProb
                                  in weight + go shorter

        getProb :: [Integer] -> Maybe Prob
        getProb ngram = let probs = map (Map.lookup ngram) allNGrams
                        in firstJust probs
                        -- thank you, lazyness!

        firstJust :: [Maybe a] -> Maybe a
        firstJust [] = Nothing
        firstJust (x : xs)
            | isNothing x = firstJust xs
            | otherwise   = x
