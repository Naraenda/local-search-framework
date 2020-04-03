module LocalSearch.Framework.SimulatedAnnealing
  ( runSA
  )
where

import System.Random

import LocalSearch.Framework.SearchProblem

type Iterations = Int
type Temperature = Score

-- | Runs the simulated annealing algorithm on the given searchable. Requires
-- an initial state, but should work on /any/ initial state. Runs for a set
-- amount of iterations, and the temperature is based on the ratio of
-- iterations left and total iterations.
runSA :: (Heuristic a, Searchable a)
      => Iterations -- ^ The max amount of iterations to run
      -> a          -- ^ The initial state
      -> IO a       -- ^ The resulting state
runSA = runSA' 0

-- | Actually runs the simulated annealing; the above is only a helper function
-- so users don't do dumb things.
runSA'  :: (Heuristic a, Searchable a)
        => Iterations -- ^ The amount of iterations already run
        -> Iterations -- ^ The max amount of iterations to run
        -> a          -- ^ The current state
        -> IO a       -- ^ The resulting state
runSA' c m s
  | c >= m    = return s
  | otherwise = do
    let t = temperature c m 0.0 100.0 -- TODO parameterize
    action <- chooseRandom $ neighbours s
    let sNew = explore s action
    r <- randomIO
    let sRes = if p (score s) (score sNew) t >= r then sNew else s
    runSA' (c + 1) m sRes

-- | Gets the probability of picking a given new state. Sometimes you need to
-- pass a worse state to get to a new, better state. This probability is based
-- on the scores of the current state and the new state, and the current
-- temperature.
p :: Score -> Score -> Temperature -> Score
p c n t
  | n > c = 1.0 -- Maximal probability; better than it was before
  | otherwise = exp $ -(c - n) / t

-- | Gets the current temperature based on the iterations and the minimal and
-- maximal temperature.
temperature :: Iterations   -- ^ The current amount of iterations
            -> Iterations   -- ^ The maximum amount of iterations
            -> Temperature  -- ^ The minimum temperature
            -> Temperature  -- ^ The maximum temperature
            -> Temperature  -- ^ The temperature for this simulation time
temperature c m tmin tmax = tmax - tdiff * timefrac
  where
    tdiff = tmax - tmin
    timefrac = fromIntegral m / fromIntegral (c + 1)

-- | Helper function to choose a random element from a list. Probably exists
-- somewhere in the standard library; in that case, we need to use that one.
-- Also, I'd prefer a non-IO monad, if at all possible.
chooseRandom :: [a] -> IO a
chooseRandom xs = do
  let len = length xs
  i <- randomRIO (0, len - 1)
  return $ xs !! i

