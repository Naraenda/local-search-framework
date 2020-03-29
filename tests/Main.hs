module Main where

import LocalSearch.Tests.Problems.Satisfiability
import LocalSearch.Framework.GeneticAlgorithm
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.Tabu 
import Data.Foldable
import Data.Map (fromList)

popSize :: PopulationSize
popSize = 10

genIterations :: Iterations
genIterations = 10

mutateProb :: Probability
mutateProb = 0.05

main :: IO ()
main = do
  (Right formula) <- readCNF "tests/cnf.txt"
  let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
  --putStrLn "Running climb"
  --solution <- runClimb $ makeTabu 10 problem
  putStrLn "Running genetic"
  solution <- (runGenetic popSize genIterations mutateProb $ formula :: IO SATProblem)
  print solution

exampleProblem :: SATProblem
exampleProblem = SP sat sol
  where
    sat = SAT [
            Clause [Var "a", Var "b"]
          , Clause [Var "c", Var "b", Var "d"]
          , Clause [Var "d"]
          ]
    sol = fromList $ (,) <$> toList (vars sat) <*> pure False

