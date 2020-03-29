{-# LANGUAGE
    MultiParamTypeClasses
  , DeriveGeneric
  , FlexibleInstances
  , DeriveAnyClass #-}
module Example where

import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.HillClimbing
import GHC.Generics(Generic)

type Var = Int
data Action = Increment | Decrement

instance Searchable Var Action where
    neighbours = const [Increment, Decrement]

    explore n Increment = n + 1
    explore n Decrement = n - 1

data Problem = Problem { x :: Var, y :: Var } deriving (Generic)

instance Searchable Problem (Either Action Action)

ourProblem :: Hr Problem
ourProblem = Problem 0 0 `withHeuristic` h
    where
        h (Problem x y) = fromIntegral $ 
            - ( x ^ 2) - ( y ^ 2) 
            + (39 * x) - (97 * y)
            + ( x * y `mod` 19)

runExample :: IO ()
runExample = do
    result <- runClimb ourProblem
    let Hr _ (Problem x y) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ " } " ++ show (score result)