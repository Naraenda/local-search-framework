module LocalSearch.Framework.SimulatedAnnealing
  ( runSA
  )
where

import System.Random

import LocalSearch.Framework.SearchProblem
  (Searchable(score, neighbours, explore), Score)

type Iterations = Int
type Temperature = Float

runSA :: Searchable a b
      => Iterations -- ^ The max amount of iterations to run
      -> a          -- ^ The initial state
      -> IO a       -- ^ The resulting state
--runClimb :: (Show a, Searchable a) => a -> IO a -- for debugging
runSA = runSA' 0

runSA'  :: Searchable a b
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

p :: Score -> Score -> Temperature -> Float
p c n t
  | n > c = 1.0 -- Maximal probability; better than it was before
  | otherwise = exp $ -(c - n) / t

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

