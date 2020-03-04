module Main where

import LocalSearch.Tests.Problems.Satisfyability
import LocalSearch.Framework.HillClimbing
import LocalSearch.Framework.Tabu 
import Data.Foldable
import Data.Map (fromList)

main :: IO ()
main = do
  (Right formula) <- readCNF "tests/cnf.txt"
  let problem = SP formula . fromList $ (,) <$> toList (vars formula) <*> pure False
  solution <- runClimb $ makeTabu 10 problem
  putStrLn . show $ solution

exampleProblem :: SATProblem
exampleProblem = SP sat sol
  where
    sat = SAT [
            Clause [Var "a", Var "b"]
          , Clause [Var "c", Var "b", Var "d"]
          , Clause [Var "d"]
          ]
    sol = fromList $ (,) <$> toList (vars sat) <*> pure False

