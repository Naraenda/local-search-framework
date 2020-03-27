module Main where

import LocalSearch.Tests.Problems.Satisfiability
import LocalSearch.Tests.Problems.TSP
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.Tabu
import LocalSearch.Framework.SimulatedAnnealing
import LocalSearch.Framework.SearchProblem
import Data.Foldable
import Data.Map (fromList)

main :: IO ()
main = do
--(Right formula) <- readTSP "tests/cnf.txt"
--let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
--solution <- runClimb $ makeTabu 10 problem
--print solution
  --parsed <- readTSP "tests/test1.tsp"
  -- parsed <- readTSP "tests/xqf131.tsp"
  -- case parsed of
  --   Left e -> print e
  --   Right tsp -> do
  --     print "Starting hillclimb"
  --     -- Randomize the problem:
  --     tsp <- shuffle tsp
  --     res <- runClimb tsp
  --     print res
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


exampleProblem :: SATProblem
exampleProblem = SP sat sol
  where
    sat = SAT [
            Clause [Var "a", Var "b"]
          , Clause [Var "c", Var "b", Var "d"]
          , Clause [Var "d"]
          ]
    sol = fromList $ (,) <$> toList (vars sat) <*> pure False




