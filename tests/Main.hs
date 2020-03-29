module Main where

import LocalSearch.Tests.Problems.Satisfiability
import LocalSearch.Tests.Problems.TSP
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.Tabu 
import Data.Foldable
import Data.Map (fromList)
import qualified Example

main :: IO ()
main = Example.runExample
{-main = do
--(Right formula) <- readTSP "tests/cnf.txt"
--let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
--solution <- runClimb $ makeTabu 10 problem
--print solution
  --parsed <- readTSP "tests/test1.tsp"
  parsed <- readTSP "tests/xqf131.tsp"
  case parsed of
    Left e -> print e
    Right tsp -> do
      print "Starting hillclimb"
      -- Randomize the problem:
      tsp <- shuffle tsp
      
      res <- runClimb tsp
      print res-}

exampleProblem :: SATProblem
exampleProblem = SP sat sol
  where
    sat = SAT [
            Clause [Var "a", Var "b"]
          , Clause [Var "c", Var "b", Var "d"]
          , Clause [Var "d"]
          ]
    sol = fromList $ (,) <$> toList (vars sat) <*> pure False
    

