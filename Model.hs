-- | Defines data structures for the n-gram model.
module Model where

import qualified Data.Map.Strict as Map

data Header = Header Int [Int] deriving Show
data Prob = Prob Double Double | ProbMax Double deriving Show
type NGrams = Map.Map [String] Prob
data Model = Model Header [NGrams] deriving Show

-- | Extracts the list of all n-grams from a model.
extractAllNGrams ::
    Model -> -- ^ source of to-be-extracted list
    [NGrams] -- ^ extracted list of n-grams
extractAllNGrams (Model _ allNGrams) = allNGrams


-- | Extracts the probability from a Prob structure.
extractProb ::
    Prob -> -- ^ source of to-be-extracted probability
    Double  -- ^ extracted probability
extractProb (Prob p _) = p
extractProb (ProbMax p) = p


-- | Extracts the backoff weight from a Prob structure.
extractBackoff ::
    Prob -> -- ^ source of to-be-extracted backoff weight
    Double  -- ^ extracted backoff weight
extractBackoff (Prob _ b) = b
extractBackoff _ = undefined
