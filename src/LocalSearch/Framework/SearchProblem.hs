module LocalSearch.Framework.SearchProblem where

import Control.Monad.Random.Lazy
import           Data.Maybe                     ( maybeToList )

type Score = Float

-- Maybe it's better to have a generic (class) definition of a search problem?
-- It makes it implementing the solver and problem both more easily.

class Searchable a where
  -- | Gives the current state a score
  score :: a -> Score

  -- | Returns all neighbouring states
  neighbours :: a -> [a]

  -- | Returns a random nieghbouring state
  randomNeighbour :: RandomGen g => a -> Rand g (Maybe a)
  randomNeighbour = uniformMay . neighbours

-- | Combines two searchable problems intro one
data Composed a b = Composed (a -> b -> Score) a b

instance (Searchable a, Searchable b) => Searchable (Composed a b) where
  score (Composed f a b) = f a b

  neighbours (Composed f a b) = 
    [Composed f a' b | a' <- neighbours a] ++
    [Composed f a b' | b' <- neighbours b]

  randomNeighbour (Composed f a b) = do
    a' <- randomNeighbour a
    b' <- randomNeighbour b
    let ca = flip (Composed f) b <$> maybeToList a'
    let cb = Composed f a <$> maybeToList b'
    uniformMay (ca ++ cb)

-- | Sums the scores of two search problems
sumScores :: (Searchable a, Searchable b) => a -> b -> Score
sumScores a b = score a + score b

-- | Multiplies the scores of two search problems
mulScores :: (Searchable a, Searchable b) => a -> b -> Score
mulScores a b = score a * score b