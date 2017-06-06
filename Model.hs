-- | Defines data structures for the n-gram model.
module Model where

import qualified Data.Map.Strict as Map

-- | Header of the model file containing the maximum length of n-grams and the
-- number of n-grams for each n.
data Header = Header Int [Int] deriving Show

-- | Probability of an n-gram and its backoff weight (unless n = nMax)
data Prob = Prob Double Double | ProbMax Double deriving Show

-- | Map of all n-grams for a fixed n. The n-gram (list of words) maps to its
-- probability and backoff weight.
type NGrams = Map.Map [String] Prob

-- | Combines the header information with the n-gram maps for each n.
data Model = Model Header [NGrams] deriving Show


-- | Extracts the list of all n-grams from a model.
extractAllNGrams
    :: Model     -- ^ source of to-be-extracted list
    -> [NGrams]  -- ^ extracted list of n-grams
extractAllNGrams (Model _ allNGrams) = allNGrams


-- | Extracts the probability from a Prob structure.
extractProb
    :: Prob    -- ^ source of to-be-extracted probability
    -> Double  -- ^ extracted probability
extractProb (Prob p _) = p
extractProb (ProbMax p) = p


-- | Extracts the backoff weight from a Prob structure.
extractBackoff
    :: Prob    -- ^ source of to-be-extracted backoff weight
    -> Double  -- ^ extracted backoff weight
extractBackoff (Prob _ b) = b
extractBackoff _ = undefined
