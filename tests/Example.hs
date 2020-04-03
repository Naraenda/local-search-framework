{-# LANGUAGE
    DeriveGeneric
  , FlexibleInstances
  , DeriveAnyClass
  , TypeFamilies 
  , TypeApplications
  , MultiParamTypeClasses #-}
module Example where

import Data.Foldable
import Data.Map (fromList)
import Data.Bits
import Data.Int
import GHC.Generics (Generic)
import Control.Monad.Random.Class (getRandom, getRandomR, uniform)

import LocalSearch.Framework.GeneticAlgorithm
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.SearchProblem
import LocalSearch.Tests.Problems.Satisfiability hiding (Var)

-- Problem composition example
type Var = Int
data VarAction = Increment | Decrement

instance Searchable Var where
    type Action Var = VarAction
    neighbours = const [Increment, Decrement]

    explore n Increment = n + 1
    explore n Decrement = n - 1

data Problem = Problem Var Var
    deriving (Generic, Searchable, Genetic)

instance Heuristic Problem where
  score (Problem x y) = fromIntegral $ 
    - ( x ^ 2) - ( y ^ 2) 
    + (39 * x) - (97 * y)
    + ( x * y `mod` 19)

ourProblem :: Problem
ourProblem = Problem 0 0

data BigProblem = Big Problem Problem
    deriving (Generic, Searchable, Genetic)

instance Heuristic BigProblem where
  score (Big (Problem x y) (Problem z w)) = fromIntegral $ 
    - (x ^ 2) - (y ^ 2) - ( z ^2) - (w ^ 2)
    + 39 * x + 58 * y + 4 * z + 4 * w

otherProblem :: BigProblem
otherProblem = Big (Problem 0 0) (Problem 0 0)

runExample :: IO ()
runExample = do
    result <- runClimb ourProblem
    let Problem x y = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ " } " ++ show (score result)
    
    result <- runClimb otherProblem
    let Big (Problem x y) (Problem z w) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ ", w: " ++ show w ++ " } " ++ show (score result)

-- The above problem but defined as a genetic problem:

data VarCrossover = VL Int | VR Int
data VarMutation = VM Int VarMutation | NVM

instance Crossover () VarCrossover where
  getRandomCrossover s = do
    constructor <- uniform [VL, VR]
    let maxR = fromIntegral $ finiteBitSize (undefined :: Int8)
    ind <- getRandomR (0, maxR)
    return $ constructor ind

instance Mutation () VarMutation where
  getRandomMutation p s = do
    doMut <- shouldMutate p
    if doMut
      then do
        let maxR = fromIntegral $ finiteBitSize (undefined :: Int8)
        v <- getRandomR (0, maxR - 1)
        VM v <$> getRandomMutation p s
      else return NVM

instance Genetic Var where
  type Mut   Var = VarMutation
  type Cross Var = VarCrossover
  type Space Var = ()

  randomIndividual _ = getRandom
  mutation m x = fromIntegral $ mutation' m (fromIntegral x)
    where
      mutation' :: Mut Var -> Int8 -> Int8
      mutation'  NVM x = x
      mutation' (VM m ms) x = mutation' ms x `complementBit` m

  crossover (VL c) x y = (x .&. maskL) .|. (y .&. maskR)
    where
      maskL :: Var
      maskL = unsafeShiftR (unsafeShiftL (complement 0) c) c
      maskR = complement maskL
  crossover c x y = fromIntegral $ (x' .&. maskR) .|. (y' .&. maskL)
    where
      x', y' :: Int16
      (i, x', y') = case c of
        VL c -> (c, fromIntegral x, fromIntegral y)
        VR c -> (c, fromIntegral y, fromIntegral x)
      maskL, maskR :: Int16
      maskL = unsafeShiftR (unsafeShiftL (complement 0) i) i
      maskR = complement maskL

-- This is a bad example. It only shows that the above example
-- can be written via a generic genetic instance. But the genetic
-- solver itself is not the proper tool for this problem, and will
-- most likely give an incorrect result.
runExampleGA :: IO ()
runExampleGA = do
    result <- runGenetic 100 4000 0.05 ((), ())
    let Problem x y = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ " } " ++ show (score result)
    
    result <- runGenetic 100 4000 0.05 (((), ()), ((), ()))
    let Big (Problem x y) (Problem z w) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ ", w: " ++ show w ++ " } " ++ show (score result)


-- GA example
popSize :: PopulationSize
popSize = 10

genIterations :: Iterations
genIterations = 10

mutateProb :: Probability
mutateProb = 0.05

runGeneticExample :: IO ()
runGeneticExample = do
  (Right formula) <- readCNF "tests/cnf.txt"
  let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
  putStrLn "Running genetic"
  solution <- runGenetic popSize genIterations mutateProb formula
  print (solution :: SATProblem)

