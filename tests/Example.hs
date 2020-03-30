{-# LANGUAGE
    DeriveGeneric
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

data Problem = Problem Var Var
    deriving (Generic, Searchable)

ourProblem :: Hr Problem
ourProblem = Problem 0 0 `withHeuristic` h
    where
        h (Problem x y) = fromIntegral $ 
            - ( x ^ 2) - ( y ^ 2) 
            + (39 * x) - (97 * y)
            + ( x * y `mod` 19)

data BigProblem = Big Problem Problem
    deriving (Generic, Searchable)

otherProblem :: Hr BigProblem
otherProblem = Big (Problem 0 0) (Problem 0 0) `withHeuristic` h
    where
        h (Big (Problem x y) (Problem z w)) = fromIntegral $ 
            - (x ^ 2) - (y ^ 2) - ( z ^2) - (w ^ 2)
            + 39 * x + 58 * y + 4 * z + 4 * w

runExample :: IO ()
runExample = do
    result <- runClimb ourProblem
    let Hr _ (Problem x y) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ " } " ++ show (score result)
    
    result <- runClimb otherProblem
    let Hr _ (Big (Problem x y) (Problem z w)) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ ", w: " ++ show w ++ " } " ++ show (score result)
