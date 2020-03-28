{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalSearch.Framework.SearchProblem where

import Control.Monad.Random.Lazy
import Data.Maybe (maybeToList)

type Score = Float

-- Maybe it's better to have a generic (class) definition of a search problem?
-- It makes it implementing the solver and problem both more easily.

class Searchable a b | a -> b where
  -- | Gives the current state a score
  score :: a -> Score

  -- | Returns all neighbouring states
  neighbours :: a -> [b]

  -- | Explore a neighbour using an action
  explore :: a -> b -> a

-- | Combines two searchable problems intro one
data Composed a b = Composed (a -> b -> Score) a b

instance (Searchable a fa, Searchable b fb) => Searchable (Composed a b) (Either fa fb) where
  score (Composed f a b) = f a b

  neighbours (Composed f a b) = (Left <$> neighbours a) ++ (Right <$> neighbours b)

  explore (Composed f a b) (Left  fa) = Composed f (explore a fa) b
  explore (Composed f a b) (Right fb) = Composed f a (explore b fb)

-- | Sums the scores of two search problems
sumScores :: (Searchable a fa, Searchable b fb) => a -> b -> Score
sumScores a b = score a + score b

-- | Multiplies the scores of two search problems
mulScores :: (Searchable a fa, Searchable b fb) => a -> b -> Score
mulScores a b = score a * score b
