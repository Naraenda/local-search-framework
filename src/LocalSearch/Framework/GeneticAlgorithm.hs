{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module LocalSearch.Framework.GeneticAlgorithm
  (
  )
where

import Control.Monad.Random.Lazy

import Data.List (sortOn)
import Data.Maybe (fromJust)

type Fitness = Double
type Iterations = Int
type PopulationSize = Int

class EnumRandom a where
  getRandomValue :: RandomGen g => Rand g a

-- | Defines a genetic algorithm on type `a`. This requires two additional
-- types, `cross`, defining what kinds of crossover can happen, and `mut`,
-- which defines what kind of mutations can happen. The user needs to implement
-- the functions crossover and mutation based on these types. The value passed
-- to these functions will be generated automatically by the solver.
class (EnumRandom cross, EnumRandom mut) =>
      GeneticAlgorithm cross mut a | a -> mut, a -> cross where
  randomIndividual  :: RandomGen g => Rand g a
  fitness           :: a -> Fitness
  crossover         :: cross -> a -> a -> a
  mutation          :: mut -> a -> a

runGenetic :: GeneticAlgorithm cross mut a => IO a
runGenetic = evalRandIO $ runGeneticRand popSize iters
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

runGeneticRand :: (GeneticAlgorithm cross mut a, RandomGen g)
               => Int
               -> Iterations
               -> Rand g a
runGeneticRand popSize iters =
  do
    pop       <- makeRandomPopulation popSize
    finalPop  <- runGenetic' iters (popSize, pop)
    return $ selectBest finalPop
  where
    selectBest = fst . fromJust . takeMax snd . map (\x -> (x, fitness x)) . snd

makeRandomPopulation :: (GeneticAlgorithm cross mut a, RandomGen g)
                     => Int
                     -> Rand g [a]
makeRandomPopulation = sequence . flip replicate randomIndividual

getParents :: (GeneticAlgorithm cross mut a, RandomGen g)
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

generateNewPopulation :: (GeneticAlgorithm cross mut a, RandomGen g)
                      => PopulationSize
                      -> [a]
                      -> Rand g [a]
generateNewPopulation popSize pop = sequence . replicate popSize $
  do
    (p1, p2) <- getParents pop
    co <- getRandomValue
    let child = crossover co p1 p2
    mut <- getRandomValue
    return $ mutation mut child

runGenetic' :: (GeneticAlgorithm cross mut a, RandomGen g)
            => Iterations
            -> (PopulationSize, [a])
            -> Rand g (PopulationSize, [a])
runGenetic' 0 x = return x
runGenetic' n (popSize, pop) =
  do
    newPop <- generateNewPopulation popSize pop
    runGenetic' (n - 1) (popSize, newPop)

