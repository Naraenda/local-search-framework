module Main where

import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.Tabu
import LocalSearch.Framework.SimulatedAnnealing
import LocalSearch.Framework.SearchProblem

import LocalSearch.Tests.Problems.Satisfiability
import LocalSearch.Tests.Problems.TSP

import Data.Foldable
import Data.Map (fromList)

import qualified Example

main :: IO ()
main = Example.runExampleGA
{-
main = do
  formula <- readCNF "tests/test1.cnf"
  case formula of
    Left err -> print err
    Right f -> do
      let problem = SP f . fromList $ (,) <$> toList (vars f) <*> pure False
      solution <- runSA 1000000 problem
      putStrLn "Solution that we found:"
      print solution

      putStrLn "Score of this solution:"
      print $ score solution
-}

exampleProblem :: SATProblem
exampleProblem = SP sat sol
  where
    sat = SAT [
            Clause [Var "a", Var "b"]
          , Clause [Var "c", Var "b", Var "d"]
          , Clause [Var "d"]
          ]
    sol = fromList $ (,) <$> toList (vars sat) <*> pure False

