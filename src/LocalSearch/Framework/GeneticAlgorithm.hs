{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module LocalSearch.Framework.GeneticAlgorithm
  ( EnumRandom(..)
  , GeneticAlgorithm(..)

  , Fitness
  , Iterations
  , PopulationSize

  , runGenetic
  )
where

import Control.Monad.Random.Lazy

import Data.List (sortOn)
import Data.Maybe (fromJust)

type Fitness = Double
type Iterations = Int
type PopulationSize = Int

class EnumRandom s a | a -> s where
  getRandomValue :: RandomGen g => s -> Rand g a

-- | Defines a genetic algorithm on type `a`. This requires two additional
-- types, `cross`, defining what kinds of crossover can happen, and `mut`,
-- which defines what kind of mutations can happen. The user needs to implement
-- the functions crossover and mutation based on these types. The value passed
-- to these functions will be generated automatically by the solver.
class (EnumRandom space cross, EnumRandom space mut) =>
      GeneticAlgorithm cross mut space a | a -> mut, a -> cross, a -> space where
  randomIndividual  :: RandomGen g => space -> Rand g a
  fitness           :: a -> Fitness
  crossover         :: cross -> a -> a -> a
  mutation          :: mut -> a -> a

runGenetic :: GeneticAlgorithm cross mut space a => space -> IO a
runGenetic space = evalRandIO $ runGeneticRand popSize iters space
  where
    popSize = 100
    iters   = 100

takeMax :: Ord a => (b -> a) -> [b] -> Maybe b
takeMax f = fmap fst . tm' Nothing f
  where
    tm' x        _ []     = x
    tm' Nothing  f (x:xs) = tm' (Just (x, f x)) f xs
    tm' a@(Just (x, v)) f (y:ys)
      | v < f y   = tm' (Just (y, f y)) f ys
      | otherwise = tm' a               f ys

runGeneticRand :: (GeneticAlgorithm cross mut space a, RandomGen g)
               => Int
               -> Iterations
               -> space
               -> Rand g a
runGeneticRand popSize iters space =
  do
    pop       <- makeRandomPopulation space popSize
    finalPop  <- runGenetic' space iters (popSize, pop)
    return $ selectBest finalPop
  where
    selectBest = fst . fromJust . takeMax snd . map (\x -> (x, fitness x)) . snd

makeRandomPopulation :: (GeneticAlgorithm cross mut space a, RandomGen g)
                     => space
                     -> Int
                     -> Rand g [a]
makeRandomPopulation s = sequence . flip replicate (randomIndividual s)

getParents :: (GeneticAlgorithm cross mut space a, RandomGen g)
           => [a]
           -> Rand g (a, a)
getParents pop = gp'
  where
    gp' =
      do
        val1 <- getRandomR (0, totalWeights)
        val2 <- getRandomR (0, totalWeights)
        return (getParent val1 popWithFitness, getParent val2 popWithFitness)
    totalWeights    = sum $ map snd $ popWithFitness
    popWithFitness  = sortOn snd $ (\x -> (x, fitness x)) <$> pop
    getParent x ((y,w):pop)
      | x <= w = y
      | otherwise = getParent (x - w) pop

generateNewPopulation :: (GeneticAlgorithm cross mut space a, RandomGen g)
                      => space
                      -> PopulationSize
                      -> [a]
                      -> Rand g [a]
generateNewPopulation space popSize pop = sequence . replicate popSize $
  do
    (p1, p2) <- getParents pop
    co <- getRandomValue space
    let child = crossover co p1 p2
    mut <- getRandomValue space
    return $ mutation mut child

runGenetic' :: (GeneticAlgorithm cross mut space a, RandomGen g)
            => space
            -> Iterations
            -> (PopulationSize, [a])
            -> Rand g (PopulationSize, [a])
runGenetic' _ 0 x = return x
runGenetic' space n (popSize, pop) =
  do
    newPop <- generateNewPopulation space popSize pop
    runGenetic' space (n - 1) (popSize, newPop)

