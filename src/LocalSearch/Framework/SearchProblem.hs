module LocalSearch.Framework.SearchProblem where

import Control.Monad.Random.Lazy

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
