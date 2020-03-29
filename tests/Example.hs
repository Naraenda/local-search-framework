{-# LANGUAGE
    MultiParamTypeClasses
  , DeriveGeneric
  , FlexibleInstances
  , DeriveAnyClass
  , TypeFamilies #-}
module Example where

import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.HillClimbing
import GHC.Generics(Generic)

type Var = Int
data VarAction = Increment | Decrement

instance Searchable Var where
    type Action Var = VarAction
    neighbours = const [Increment, Decrement]

    explore n Increment = n + 1
    explore n Decrement = n - 1

data Problem = Problem { x :: Var, y :: Var } deriving (Generic)

instance Searchable Problem where
    type Action Problem = Either VarAction VarAction

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