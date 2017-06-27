-- | Defines data structures for the n-gram model and corresponding, useful
-- functions.
module Model (module Model) where

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map

-- | Header of the model file containing meta information.
data Header = Header
    { headerNMax :: Int    -- ^ max length of n-grams
    , headerNums :: [Int]  -- ^ numbers of n-grams for each n
    } deriving Show

-- | Probability of an n-gram and its backoff weight.
data Prob = Prob
    { probability :: Double    -- ^ probability
    , backoff :: Maybe Double  -- ^ backoff weight, unless n is maximal
    } deriving Show

-- | Maps n-grams for a fixed n (that have been encoded as integers)
-- to their probabilities (and backoff weights).
type NGrams
    = Map.Map [Int] Prob  -- ^ (converted n-gram) -\> probability

-- | Data structure for building a UniMap structure.
data MapBuilder = Builder
      { nextInt :: Int     -- ^ next integer to use
      , currMap :: UniMap  -- ^ current mapping
      } deriving Show

-- | Bijective mapping of unigrams to their integer representation / encoding.
type UniMap =
    Bimap.Bimap String Int  -- ^ mapping: unigram \<-\> integer

-- | Combines header, n-grams and integer mapping.
data Model = Model
    { modelHeader :: Header    -- ^ header of the model
    , modelNGrams :: [NGrams]  -- ^ list of all n-gram maps
    , modelUniMap :: UniMap    -- ^ unigram mapping
    } deriving Show
