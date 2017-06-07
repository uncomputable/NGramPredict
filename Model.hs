-- | Defines data structures for the n-gram model and corresponding, useful
-- functions.
module Model (module Model) where

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


-- | Extracts the list of all maps of n-gram from a model.
extractAllNGrams
    :: Model     -- ^ source of extraction
    -> [NGrams]  -- ^ extracted list of n-grams
extractAllNGrams (Model _ allNGrams) = allNGrams


-- | Extracts the header from a model.
extractHeader
    :: Model   -- ^ source of extraction
    -> Header  -- ^ extracted header
extractHeader (Model header _) = header


-- | Extracts the length of the longest n-grams from the header of a model.
headerGetNMax
    :: Header  -- ^ source of extraction
    -> Int     -- ^ extracted longest length
headerGetNMax (Header nMax _) = nMax


-- | Extracts the probability from a Prob structure.
extractProb
    :: Prob    -- ^ source of extraction
    -> Double  -- ^ extracted probability
extractProb (Prob p _) = p
extractProb (ProbMax p) = p


-- | Extracts the backoff weight from a Prob structure.
extractBackoff
    :: Prob    -- ^ source of extraction
    -> Double  -- ^ extracted backoff weight
extractBackoff (Prob _ b) = b
extractBackoff _ = undefined
