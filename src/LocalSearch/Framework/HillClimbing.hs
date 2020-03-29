{-# LANGUAGE AllowAmbiguousTypes #-}

module LocalSearch.Framework.HillClimbing
  ( runClimb
  )
where

import System.Random
import Control.Monad.Random.Lazy (evalRandIO)
import Data.Maybe(catMaybes)

import LocalSearch.Framework.SearchProblem

-- | Runs a hill climbing algorithm on a `Searchable`. Uses the `neighbours`
-- function of the Searchable to discover new states, and randomly selects a
-- better one, if it exists. If the `score` of the new state is lower than or
-- equal to the one of the old state, it discards the new state. Otherwise,
-- this will take the new state. This stops when a local optimum is found.
--
-- This function should probably get a different signature later. I'd prefer
-- not having an `IO` requirement on this, but rather a different monad that
-- allows for randomness to happen, even if calling it requires IO afterwards.
runClimb :: (Heuristic s, Searchable s) => s -> IO s
runClimb x = do
  let s = score x
  let ns = explore x <$> neighbours x
  let nsScores = score <$> ns
  let newStates = filter ((>s) . score) ns

  case newStates of
    [] -> return x
    _  -> chooseRandom newStates >>= runClimb

-- | Helper function to choose a random element from a list. Probably exists
-- somewhere in the standard library; in that case, we need to use that one.
-- Also, I'd prefer a non-IO monad, if at all possible.
chooseRandom :: [a] -> IO a
chooseRandom xs = (xs!!) <$> randomRIO (0, length xs - 1)

