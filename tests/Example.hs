{-# LANGUAGE MultiParamTypeClasses #-}
module Example where

import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.HillClimbing

newtype State = Var Int
data Actions = Increment | Decrement

instance Searchable State Actions where
    score (Var n) = fromIntegral n

    explore (Var n) Increment = Var (n + 1)
    explore (Var n) Decrement = Var (n - 1)

    neighbours = const [Increment, Decrement]

ourProblem :: Composed State State
ourProblem = Composed h (Var 0) (Var 0)
    where
        h (Var x) (Var y) = fromIntegral $ 
            - ( x ^ 2) - ( y ^ 2) 
            + (39 * x) - (97 * y)
            + ( x * y `mod` 19)

runExample :: IO ()
runExample = do
    result <- runClimb ourProblem
    let Composed _ (Var x) (Var y) = result
    putStrLn $ "{ x: " ++ show x ++ ", y: " ++ show y ++ " } " ++ show (score result)