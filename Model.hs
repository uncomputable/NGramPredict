-- | Defines data structures for the n-gram model and corresponding, useful
-- functions.
module Model (module Model) where

import qualified Data.Map.Strict as Map

-- | Header of the model file containing the maximum length of n-grams and the
-- number of n-grams for each n.
data Header
    = Header Int [Int]  -- ^ (max length) (numbers of n-grams)
    deriving Show

-- | Probability of an n-gram and its backoff weight (unless n = nMax)
data Prob
    = Prob Double Double  -- ^ probability (backoff weight)
    | ProbMax Double      -- ^ probability
    deriving Show

-- | Maps n-grams for a fixed n (that have been converted to an integer
-- representation) to their probabilities (and backoff weights).
type NGrams
    = Map.Map [Integer] Prob  -- ^ (converted n-gram) -> probability

-- | Data structure for building a UniMap structure.
data MapBuilder = Builder
      { nextInt :: Integer  -- ^ next integer to use
      , currMap :: UniMap   -- ^ current mapping
      } deriving Show

-- | Bijective mapping of unigrams to their integer representation.
type UniMap =
    Map.Map String Integer  -- ^ mapping: unigram -> integer

-- | Combines header, n-grams and integer mapping.
data Model
    = Model Header [NGrams] UniMap  -- ^ (model header) (all n-grams) (unigram mapping)
    deriving Show


-- | Extracts the list of all maps of n-gram from a model.
extractAllNGrams
    :: Model     -- ^ source of extraction
    -> [NGrams]  -- ^ extracted list of n-grams
extractAllNGrams (Model _ allNGrams _) = allNGrams


-- | Extracts the header from a model.
extractHeader
    :: Model   -- ^ source of extraction
    -> Header  -- ^ extracted header
extractHeader (Model header _ _) = header


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
