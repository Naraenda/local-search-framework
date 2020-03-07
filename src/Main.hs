module Main where

import LocalSearch.Tests.Problems.Satisfyability
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.SimulatedAnnealing
import Data.Foldable
import Data.Map (fromList)

main :: IO ()
main = do
  (Right formula) <- readCNF "tests/cnf.txt"
  let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
  solution <- runSA 1000000 problem
  putStrLn "Solution that we found:"
  putStrLn . show $ solution

      putStrLn "Score of this solution:"
      putStrLn . show $ score solution

