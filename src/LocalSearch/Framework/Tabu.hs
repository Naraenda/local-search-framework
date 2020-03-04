module LocalSearch.Framework.Tabu 
  ( Tabuable(..)
  , Tabu(..)
  , makeTabu
  ) where

import LocalSearch.Framework.SearchProblem

-- | Defines a searchable space where a Tabu' list can be built in
class Searchable a => Tabuable a where
  fingerprint :: Eq b => a -> b

-- | Wraps a state s with a list of t tabu's. If s is tabuable, then
-- its neighbours do not include elements from the tabu list.
data Tabu s t = Tabu 
  { state :: s 
  , tabuList :: [t]
  , tabuSize :: Int }

-- | Wraps state s into a 'Tabu' with a maximum of n tabu's.
makeTabu :: (Eq t, Tabuable s) => Int -> s -> Tabu s t
makeTabu n s = Tabu s [] n

instance (Eq t, Tabuable s) => Tabuable (Tabu s t) where
  fingerprint = fingerprint . state

instance (Eq t, Tabuable s) => Searchable (Tabu s t) where
  score = score . state
  neighbours s@(Tabu _ l _) = 
    -- Update 'Tabu' data:
      fmap (replaceState s)
    -- Exlcude neighbours in tabu list:
    . filter ((`elem` l) . fingerprint)
    -- Get neighbouring states:
    . neighbours $ state s

-- | Replaces the internal state of 'Tabu' and adds the old state
-- to the tabu list.
replaceState :: (Eq t, Tabuable s) =>  Tabu s t -> s -> Tabu s t
replaceState t@(Tabu s l n) x = Tabu x l' n
  where
    l' = fingerprint t : drop (length l - n + 1) l