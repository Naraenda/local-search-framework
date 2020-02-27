module LocalSearch.Framework.HillClimbing
  ( SearchState(SState)
  , runClimb
  )
where

-- Completely bogus state just for random tests
data SearchState = SState { getScore :: Int }

-- | Takes the current `SearchState` and returns a new state. This new state is
-- then used to see if this mutation was better. This new mutated state is used
-- in `runClimb`; if this new state was better, we keep the new state, and
-- otherwise we keep the old state.
--
-- We should probably change this to not return an `IO State`, but a stricter
-- type that only allows drawing random values.
mutateState :: SearchState -> IO SearchState
mutateState = undefined

-- | A scoring function for the given state. Used to determine whether one
-- `SearchState` is better or worse than another.
scoreState :: SearchState -> Float
scoreState = fromIntegral . getScore

runClimb :: SearchState -> IO SearchState
runClimb = undefined

