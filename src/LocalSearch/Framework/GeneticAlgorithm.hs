{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , FlexibleContexts
  , FlexibleInstances
  , DefaultSignatures #-}
module LocalSearch.Framework.GeneticAlgorithm
  ( Mutation(..)
  , Crossover(..)
  , Genetic(..)

  , Fitness
  , Iterations
  , PopulationSize
  , Probability
  , runGenetic
  , shouldMutate
  )
where

import Control.Monad.Random.Lazy
import LocalSearch.Framework.SearchProblem(Heuristic(..), Score, Hr(..))
import GHC.Generics

import Data.List (sortOn)
import Data.Maybe (fromJust)

type Fitness = Score
type Iterations = Int
type PopulationSize = Int
type Probability = Double -- ^ A `Double` in range [0.0, 1.0)

fitness :: Heuristic a => a -> Fitness
fitness = score

class Mutation s a | a -> s where
  -- | Generates a random mutation, based on the given probability of mutation.
  getRandomMutation :: RandomGen g
                    => Probability -- ^ The probability of mutation.
                    -> s          -- ^ The space of the problem instance.
                    -> Rand g a   -- ^ The resulting mutation.

instance (Mutation sa a, Mutation sb b) => Mutation (sa, sb) (a, b) where
  getRandomMutation p (sa, sb) = liftM2 (,) (getRandomMutation p sa) (getRandomMutation p sb) 

-- | Checks whether a mutation should occur, given a probability of mutation.
shouldMutate :: RandomGen g => Probability -> Rand g Bool
shouldMutate p = (< p) <$> getRandomR (0.0, 1.0)

class Crossover s a | a -> s where
  getRandomCrossover :: RandomGen g => s -> Rand g a

instance (Crossover sa a, Crossover sb b) => Crossover (sa, sb) (a, b) where
  getRandomCrossover (sa, sb) = liftM2 (,) (getRandomCrossover sa) (getRandomCrossover sb)

class (Genetic a, Mutation (Space a) (Mut a), Crossover (Space a) (Cross a)) => GeneticSolvable a
instance (Genetic a, Mutation (Space a) (Mut a), Crossover (Space a) (Cross a)) => GeneticSolvable a
-- | Defines a genetic algorithm on type `a`. This requires two additional
-- types, `cross`, defining what kinds of crossover can happen, and `mut`,
-- which defines what kind of mutations can happen. The user needs to implement
-- the functions crossover and mutation based on these types. The value passed
-- to these functions will be generated automatically by the solver.
class Genetic a where
  type Cross a :: *
  type Cross a = GCross (Rep a)
  type Mut   a :: *
  type Mut   a = GMut (Rep a)
  type Space a :: *
  type Space a = GSpace (Rep a)
  
  randomIndividual  :: RandomGen g => Space a -> Rand g a
  default randomIndividual ::  (Generic a, GGenetic (Rep a), Space a ~ GSpace (Rep a)) 
    => RandomGen g => Space a -> Rand g a
  randomIndividual s = to <$> grandomIndividual s

  crossover         :: Cross a -> a -> a -> a
  default crossover :: (Generic a, GGenetic (Rep a), Cross a ~ GCross (Rep a)) 
    => Cross a -> a -> a -> a
  crossover c x y = to $ gcrossover c (from x) (from y)

  mutation          :: Mut a -> a -> a
  default mutation  :: (Generic a, GGenetic (Rep a), Mut a ~ GMut (Rep a))
    => Mut a -> a -> a
  mutation m x = to $ gmutation m (from x)

-- Generic genetic
class GGenetic f where
  type GCross f
  type GMut   f
  type GSpace f

  grandomIndividual :: RandomGen g => GSpace f -> Rand g (f a)
  gcrossover :: GCross f -> f a -> f a -> f a
  gmutation  :: GMut   f -> f a -> f a

instance GGenetic a => GGenetic (M1 i c a) where
  type GCross (M1 i c a) = GCross a
  type GMut   (M1 i c a) = GMut   a
  type GSpace (M1 i c a) = GSpace a

  grandomIndividual s        = M1 <$> grandomIndividual s
  gcrossover f (M1 a) (M1 b) = M1  $  gcrossover f a b
  gmutation  f (M1 a)        = M1  $  gmutation  f a

instance (GGenetic a, GGenetic b) => GGenetic (a :*: b) where
  type GCross (a :*: b) = (GCross a, GCross b)
  type GMut   (a :*: b) = (GMut   a, GMut   b)
  type GSpace (a :*: b) = (GSpace a, GSpace b)

  grandomIndividual (a, b) = 
    (:*:) <$> grandomIndividual a <*> grandomIndividual b
  gcrossover (fl, fr) (al :*: ar) (bl :*: br) = 
    gcrossover fl al bl :*: gcrossover fr ar br
  gmutation  (fl, fr) (al :*: ar) = 
    gmutation fl al :*: gmutation fr ar

instance (Genetic a) => GGenetic (K1 i a) where
  type GCross (K1 i a) = Cross a
  type GMut   (K1 i a) = Mut   a
  type GSpace (K1 i a) = Space a

  grandomIndividual s        = K1 <$> randomIndividual s
  gcrossover f (K1 a) (K1 b) = K1  $  crossover f a b
  gmutation  f (K1 a)        = K1  $  mutation  f a

runGenetic  :: (GeneticSolvable a, Heuristic a)
            => PopulationSize
            -> Iterations
            -> Probability
            -> Space a
            -> IO a
runGenetic popSize iters p space = evalRandIO $ runGeneticRand popSize iters p space

takeMax :: Ord a => (b -> a) -> [b] -> Maybe b
takeMax f = fmap fst . tm' Nothing f
  where
    tm' x        _ []     = x
    tm' Nothing  f (x:xs) = tm' (Just (x, f x)) f xs
    tm' a@(Just (x, v)) f (y:ys)
      | v < f y   = tm' (Just (y, f y)) f ys
      | otherwise = tm' a               f ys

runGeneticRand :: (GeneticSolvable a, Heuristic a, RandomGen g)
               => PopulationSize
               -> Iterations
               -> Probability
               -> Space a
               -> Rand g a
runGeneticRand popSize iters p space =
  do
    pop       <- makeRandomPopulation space popSize
    finalPop  <- runGenetic' space iters p (popSize, pop)
    return $ selectBest finalPop
  where
    selectBest = fst . fromJust . takeMax snd . map (\x -> (x, fitness x)) . snd

makeRandomPopulation :: (GeneticSolvable a, RandomGen g)
                     => Space a
                     -> PopulationSize
                     -> Rand g [a]
makeRandomPopulation s = sequence . flip replicate (randomIndividual s)

getParents :: (Genetic a, Heuristic a, RandomGen g)
           => [a]
           -> Rand g (a, a)
getParents pop = gp'
  where
    gp' =
      do
        val1 <- getRandomR (0, totalWeights)
        val2 <- getRandomR (0, totalWeights)
        return (getParent val1 popWithFitness, getParent val2 popWithFitness)
    totalWeights    = sum $ map snd popWithFitness
    popWithFitness  = sortOn snd $ (\x -> (x, fitness x)) <$> pop
    getParent x [(y,w)] = y
    getParent x ((y,w):pop)
      | x <= w = y
      | otherwise = getParent (x - w) pop

generateNewPopulation :: (GeneticSolvable a, RandomGen g, Heuristic a)
                      => Space a
                      -> Probability
                      -> PopulationSize
                      -> [a]
                      -> Rand g [a]
generateNewPopulation space p popSize pop = replicateM popSize $
  do
    (p1, p2) <- getParents pop
    co <- getRandomCrossover space
    let child = crossover co p1 p2
    mut <- getRandomMutation p space
    return $ mutation mut child

runGenetic' :: (GeneticSolvable a, RandomGen g, Heuristic a)
            => Space a
            -> Iterations
            -> Probability
            -> (PopulationSize, [a])
            -> Rand g (PopulationSize, [a])
runGenetic' _ 0 _ x = return x
runGenetic' space n p (popSize, pop) =
  do
    newPop <- generateNewPopulation space p popSize pop
    runGenetic' space (n - 1) p (popSize, newPop)

