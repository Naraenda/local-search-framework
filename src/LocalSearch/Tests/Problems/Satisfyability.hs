module LocalSearch.Tests.Problems.Satisfyability 
  (SATProblem, SAT, Clause, Variable, Solution) where

import Data.List(intercalate)
import Data.Set(Set, unions, singleton)
import Data.Map(Map, (!), adjust, keys)
import LocalSearch.Framework.SearchProblem
import Test.QuickCheck

-- Problem definition

-- | A satisfyability problem consisting of conjunct 'Clause's.
data SAT = SAT [Clause]

-- | A clause consisting of disjunct 'Variable's
data Clause = Clause [Variable]

-- | A variable that might be negated.
data Variable
  = Var String
  | Not String

type Solution = Map String Bool

-- | A class that defines (partial) evaluations of a SAT problem.
class Evaluable a where
  eval :: Solution -> a -> (Bool, Int)
  vars :: a -> Set String

-- Evaluable instances

instance Evaluable SAT where
  eval e (SAT sat) = foldl f (True, 0) sat
    where 
      f a b = merge a $ eval e b
      merge (a0, b0) (a1, b1) = (a0 && a1, b0 + b1)
  vars (SAT x) = unions $ fmap vars x

instance Evaluable Clause where
  eval e (Clause x) = check $ foldl f (True, 0) x
    where 
      f a b = merge a $ eval e b
      merge (a0, b0) (a1, b1) = (a0 || a1, 0)
      check (x, _) = (x, if x then 1 else 0)
  vars (Clause x) = unions $ fmap vars x

instance Evaluable Variable where
  eval e (Not x) = (not $ e ! x, 0)
  eval e (Var x) = (      e ! x, 0)
  vars (Not x) = singleton x
  vars (Var x) = singleton x

-- Arbitrary instances

instance Arbitrary SAT where
  arbitrary = do
    x <- listOf1 (arbitrary :: Gen Clause)
    return $ SAT x

instance Arbitrary Clause where
  arbitrary = do 
    x <- resize 3 $ listOf1 (arbitrary :: Gen Variable)
    return $ Clause x
    
instance Arbitrary Variable where
  arbitrary = do
    c <- elements [Not, Var]
    n <- getSize
    x <- choose ('a', ['b'..'z']!!n)
    return $ c [x]

-- Show instances

instance Show SAT where
  show (SAT x) = intercalate " & " (show <$> x)

instance Show Clause where
  show (Clause x) = "(" ++ intercalate "|" (show <$> x) ++ ")"

instance Show Variable where
  show (Not x) = "-" ++ x
  show (Var x) = x
  
-- Search state definition

-- | A satisfyability problem with an associated possible solution.
data SATProblem = SP SAT Solution

instance Searchable SATProblem where
  score (SP f x) = fromIntegral . snd $ eval x f
  neighbours (SP f x) = [SP f $ adjust not i x | i <- keys x]