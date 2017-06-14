-- | Defines data structures for the n-gram model and corresponding, useful
-- functions.
module Model (module Model) where

import qualified Data.Map.Strict as Map

-- | Header of the model file containing meta information.
data Header = Header
    { headerNMax :: Int    -- ^ max length of n-grams
    , headerNums :: [Int]  -- ^ numbers of n-grams for each n
    } deriving Show

-- | Probability of an n-gram and its backoff weight (unless n = nMax)
data Prob
    = Prob
    { problty :: Double  -- ^ probability
    , backoff :: Double  -- ^ backoff weight
    }
    | ProbMax
    { problty :: Double  -- ^ probability
    } deriving Show

-- | Maps n-grams for a fixed n (that have been encoded as integers)
-- to their probabilities (and backoff weights).
type NGrams
    = Map.Map [Integer] Prob  -- ^ (converted n-gram) -> probability

-- | Data structure for building a UniMap structure.
data MapBuilder = Builder
      { nextInt :: Integer  -- ^ next integer to use
      , currMap :: UniMap   -- ^ current mapping
      } deriving Show

-- | Bijective mapping of unigrams to their integer representation / encoding.
type UniMap =
    Map.Map String Integer  -- ^ mapping: unigram -> integer

-- | Combines header, n-grams and integer mapping.
data Model = Model
    { modelHeader :: Header    -- ^ header of the model
    , modelNGrams :: [NGrams]  -- ^ list of all n-gram maps
    , modelUniMap :: UniMap    -- ^ unigram mapping
    } deriving Show
