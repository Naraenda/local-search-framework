module Main where

import LocalSearch.Tests.Problems.Satisfyability
import LocalSearch.Framework.HillClimbing
import Data.Foldable
import Data.Map (fromList)

main :: IO ()
main = do
  let problem = exampleProblem
  solution <- runClimb problem
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

