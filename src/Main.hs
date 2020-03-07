module Main where

import LocalSearch.Tests.Problems.Satisfyability
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.SimulatedAnnealing
import Data.Foldable
import Data.Map (fromList)

main :: IO ()
main = do
  formula <- readCNF "tests/test1.cnf"
  case formula of
    Left err -> putStrLn $ show err
    Right f -> do
      let problem = SP f . fromList $ (,) <$> toList (vars f) <*> pure False
      solution <- runSA 1000000 problem
      putStrLn "Solution that we found:"
      putStrLn . show $ solution

      putStrLn "Score of this solution:"
      putStrLn . show $ score solution

