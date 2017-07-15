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
            let allNGrams = modelNGrams model
                uniGrams  = Map.keys $ head allNGrams
                uniMap    = modelUniMap model
                uniProbs  =
                    map (\[u] -> (computeProb u encPrefix allNGrams uniMap, u)) uniGrams
                      --  ^^^ non-exhaustive pattern matching?
            in map snd $ take n $ sortBy (flip compare) uniProbs


-- | Computes the probability of a word following a specific prefix, using a
-- specific language model.
computeProb
    :: Int       -- ^ word w_i following prefix (encoded)
    -> [Int]     -- ^ prefix p (encoded)
    -> [NGrams]  -- ^ list of maps of n-grams
    -> UniMap    -- ^ mapping of unigrams to integers
    -> LogFloat  -- ^ probability (w_i | p)
computeProb w_i fullPrefix allNGrams uniMap =
    let maybeUnkProb = Bimap.lookup "<unk>" uniMap >>= Just . return
                        >>= (`Map.lookup` head allNGrams) >>= Just . probability
        unkWeight    = fromMaybe (logFloat 0) maybeUnkProb
                     -- if <unk> is contained in the model, its probability
                     -- is used; otherwise 0 is used
    in go (fullPrefix ++ [w_i]) unkWeight
    where
        go :: [Int] -> LogFloat -> LogFloat
        go ngram unk = let maybeProb = getProb ngram
                       in maybe (goBackoff ngram unk) probability maybeProb

        goBackoff :: [Int] -> LogFloat -> LogFloat
        goBackoff [] unk = unk -- unigram w_i was not in model at all!
        goBackoff (_ : shorter) unk = let maybeProb = getProb shorter
                                          weight    = fromJust
                                            $ maybe (Just unk) backoff maybeProb
                                      in weight * go shorter unk

        getProb :: [Int] -> Maybe Prob
        getProb ngram = let probs = map (Map.lookup ngram) allNGrams
                        in (listToMaybe . catMaybes) probs
                        -- thank you, lazyness!
