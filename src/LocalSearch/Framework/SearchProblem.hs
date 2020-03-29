{-# LANGUAGE
    UndecidableInstances
  , FunctionalDependencies
  , FlexibleInstances
  , TypeOperators
  , DefaultSignatures
  , FlexibleContexts #-}
module LocalSearch.Framework.SearchProblem where

import Control.Monad.Random.Lazy
import Data.Maybe (maybeToList)

import GHC.Generics

type Score = Float

class Heuristic a where
  score :: a -> Score

class Searchable a b | a -> b where
  -- | Returns all neighbouring states
  neighbours :: a -> [b]

  default neighbours :: (Generic a, GSearchable (Rep a) b) => a -> [b]
  neighbours = gneighbors . from

  -- | Explore a neighbour using an action
  explore :: a -> b -> a

  default explore :: (Generic a, GSearchable (Rep a) b) => a -> b -> a
  explore a f = to $ gexplore (from a) f

-- | Adds an heuristic to a search problem
data Hr a = Hr (a -> Score) a

instance Heuristic (Hr a) where
  score (Hr f x) = f x

instance Searchable s a => Searchable (Hr s) a where
  neighbours (Hr f x) = neighbours x
  explore (Hr f x) a  = Hr f $ explore x a

instance Show a => Show (Hr a) where
  show (Hr f x) = show x

-- | Adds an heuristic to a search problem
withHeuristic :: a -> (a -> Score) -> Hr a
withHeuristic = flip Hr

-- Generic search

class GSearchable f b | f -> b where
  gneighbors :: f a -> [b]
  gexplore :: f a -> b -> f a

instance GSearchable a f => GSearchable (M1 i c a) f where
  gneighbors (M1 x) = gneighbors x
  gexplore (M1 x) f = M1 $ gexplore x f

instance (GSearchable a fa, GSearchable b fb) => GSearchable (a :*: b) (Either fa fb) where
  gneighbors (a :*: b) = (Left <$> gneighbors a) ++ (Right <$> gneighbors b)
  gexplore (a :*: b) (Left  f) = gexplore a f :*: b
  gexplore (a :*: b) (Right f) = a :*: gexplore b f

{-
-- We don't know how to generate a searchable for the data:
--   State = Foo | Bar | Quuz Bool
-- Even if we know how to generate the neighbours of Foo and Bar,
-- we don't know when and how to switch between them. In this case,
-- it is impossible to know how to switch from Foo to Quuz, since 
-- we need a way to generate the Bool

instance (GSearchable a fa, GSearchable b fb) => GSearchable (a :+: b) (Either fa fb) where
  gneighbors (L1 x) = Left  <$> gneighbors x
  gneighbors (R1 x) = Right <$> gneighbors x

  gexplore (L1 x) (Left  f) = L1 $ gexplore x f
  gexplore (R1 x) (Right f) = R1 $ gexplore x f
-}

instance (Searchable a f) => GSearchable (K1 i a) f  where
  gneighbors (K1 s) = neighbours s
  gexplore (K1 s) f = K1 $ explore s f

instance GSearchable U1 () where
  gneighbors _ = []
  gexplore _ _ = U1
