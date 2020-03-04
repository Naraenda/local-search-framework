{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module LocalSearch.Framework.Tabu 
  ( Tabuable(..)
  , Tabu(..)
  , makeTabu
  ) where

import LocalSearch.Framework.SearchProblem

-- | Defines a searchable space where a Tabu' list can be built in
class (Searchable s, Eq t) => Tabuable s t | s -> t where
  fingerprint :: s -> t

-- | Wraps a state s with a list of t tabu's. If s is tabuable, then
-- its neighbours do not include elements from the tabu list.
data Tabu s t = Tabu 
  { state :: s 
  , tabuList :: [t]
  , tabuSize :: Int }

-- | Wraps state s into a 'Tabu' with a maximum of n tabu's.
makeTabu :: (Tabuable s t) => Int -> s -> Tabu s t
makeTabu n s = Tabu s [] n

instance Show s => Show (Tabu s t) where
  show = show . state

instance (Tabuable s  t) => Tabuable (Tabu s t) t where
  fingerprint = fingerprint . state

instance (Tabuable s t) => Searchable (Tabu s t) where
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
replaceState :: (Tabuable s t) =>  Tabu s t -> s -> Tabu s t
replaceState t@(Tabu s l n) x = Tabu x l' n
  where
    l' = fingerprint t : drop (length l - n + 1) l