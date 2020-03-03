module LocalSearch.Framework.SearchProblem where

type Score = Float

-- Maybe it's better to have a generic (class) definition of a search problem?
-- It makes it implementing the solver and problem both more easily.

class Searchable a where
  -- | Gives the current state a score
  score :: a -> Score
  -- | Returns all neighbouring states
  neighbours :: a -> [a]

