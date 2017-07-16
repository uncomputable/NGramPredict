-- | Contains functions for computing probabilities using a language model.
module Probability (predict) where

import Model
import qualified Data.Bimap as Bimap
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Number.LogFloat (logFloat, LogFloat)

-- | Returns the n most probable unigrams following a given prefix and using
-- a specific language model.
predict
    :: Int       -- ^ number of predictions n
    -> [String]  -- ^ prefix p
    -> Model     -- ^ language model
    -> [String]  -- ^ list of n unigrams u with highest p(u | p)
predict n prefix model =
    let mapping       = modelUniMap model
        encPrefix     = map (fromJust . (`Bimap.lookup` mapping)) prefix
        encPrediction = go encPrefix
    in map (fromJust . (`Bimap.lookupR` mapping)) encPrediction
    where
        go :: [Int] -> [Int]
        go encPrefix =
            let allNGrams    = modelNGrams model
                uniGrams     = Map.keys $ head allNGrams
                maybeUnkProb = Bimap.lookup "<unk>" (modelUniMap model)
                                >>= Just . return
                                >>= (`Map.lookup` head allNGrams)
                                >>= Just . probability
                unkProb      = fromMaybe (logFloat 0) maybeUnkProb
                             -- if <unk> is contained in the model, its
                             -- probability is used; otherwise 0 is used
                uniProbs     =
                    map (\[u] -> (computeProb u encPrefix allNGrams unkProb, u)) uniGrams
                      --  ^^^ non-exhaustive pattern matching?
            in map snd $ take n $ sortBy (flip compare) uniProbs


-- | Computes the probability of a word following a specific prefix, using a
-- specific language model.
computeProb
    :: Int       -- ^ word w_i following prefix (encoded)
    -> [Int]     -- ^ prefix p (encoded)
    -> [NGrams]  -- ^ list of maps of n-grams
    -> LogFloat  -- ^ default probability for unknown values
    -> LogFloat  -- ^ probability (w_i | p)
computeProb w_i fullPrefix allNGrams unk = go (fullPrefix ++ [w_i])
    where
        go :: [Int] -> LogFloat
        go ngram = let maybeProb = getProb ngram
                   in maybe (goBackoff ngram) probability maybeProb

        goBackoff :: [Int] -> LogFloat
        goBackoff [] = unk -- unigram w_i was not in model at all!
        goBackoff (_ : shorter) = let maybeProb = getProb shorter
                                      weight    = fromJust
                                        $ maybe (Just unk) backoff maybeProb
                                  in weight * go shorter

        getProb :: [Int] -> Maybe Prob
        getProb ngram = let probs = map (Map.lookup ngram) allNGrams
                        in (listToMaybe . catMaybes) probs
                        -- thank you, lazyness!
