-- | Defines data structures for the n-gram model.
module Model where

import qualified Data.Map.Strict as Map

data Header = Header Int [Int] deriving Show
data Prob = Prob Double Double | ProbMax Double deriving Show
type NGrams = Map.Map [String] Prob
data Model = Model Header [NGrams] deriving Show
