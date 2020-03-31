{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances
  , TypeFamilies #-}
module LocalSearch.Tests.Problems.Satisfiability 
  ( SATProblem(..)
  , SAT(..)
  , Clause(..)
  , Variable(..)
  , Solution(..)
  , Evaluable(..)

  , readCNF
  )
  where

import Control.Monad.Random.Lazy hiding (fromList)

import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Set (Set, unions, singleton)
import Data.Map (Map, (!), adjust, keys, fromList)
import qualified Data.Map as M (toList)
import Data.Maybe (catMaybes)

import Test.QuickCheck

import Text.Parsec

import LocalSearch.Framework.GeneticAlgorithm
import LocalSearch.Framework.SearchProblem
import LocalSearch.Framework.Tabu

-- Problem definition

-- | A satisfyability problem consisting of conjunct 'Clause's.
newtype SAT = SAT [Clause]

-- | A clause consisting of disjunct 'Variable's
newtype Clause = Clause [Variable]

-- | A variable that might be negated.
data Variable
  = Var String
  | Not String

-- | Solutions to the SAT problems is a string variable, and the
-- value of that variable.
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
  eval e (Clause x) = check $ foldl f (False, 0) x
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
  deriving (Show)

-- | All the actions that are possible on a satisfyability problem.
newtype SATActions = Flip String

instance Heuristic SATProblem where
  score (SP f x) = fromIntegral . snd $ eval x f

instance Searchable SATProblem where
  type Action SATProblem = SATActions
  explore (SP f x) (Flip a) = SP f $ adjust not a x 
  neighbours (SP f x) = Flip <$> keys x

-- We should match tabu on the solution. Solution is a map, and
-- has instance Eq.
instance Tabuable SATProblem Solution where
  fingerprint (SP _ x) = x

-- Genetic instance
-- | The indices here are inclusive, and the numbers must be in range of the list. This means that /some/ crossing over will always happen.
data SATCrossOver = LeftOf Int | RightOf Int
data SATMutation = Mut Int SATMutation | EmptyMut

instance Crossover SAT SATCrossOver where
  getRandomCrossover s = do
    v <- getRandom
    let constructor = if v then LeftOf else RightOf
    let maxR = length $ vars s
    ind <- getRandomR (0, maxR)
    return $ constructor ind

instance Mutation SAT SATMutation where
  getRandomMutation p s =
    do
      doMut <- shouldMutate p
      if doMut
        then do
          v <- getRandomR (0, length (vars s) - 1)
          Mut v <$> getRandomMutation p s
        else return EmptyMut

instance GeneticAlgorithm SATCrossOver SATMutation SAT SATProblem where
  randomIndividual s  = SP s <$> sol
    where
      vs = toList $ vars s
      
      sol :: RandomGen g => Rand g Solution
      sol = fromList <$> traverse sequence (zip vs (repeat randBool))

      randBool :: RandomGen g => Rand g Bool
      randBool = getRandom

  fitness (SP f x)    = fromIntegral . snd $ eval x f
  mutation m x        = mutateSAT m x
  crossover co p1 p2  = crossoverSAT co p1 p2

mutateSAT :: SATMutation -> SATProblem -> SATProblem
mutateSAT EmptyMut x  = x
mutateSAT (Mut i m) (SP f x) =
    mutateSAT m . SP f $ invertMapAt i x
  where
    invertMapAt i = fromList . invertListAt i . M.toList
    invertListAt 0 (x:xs) = second not x : xs
    invertListAt i (x:xs) = x : invertListAt (i - 1) xs
  -- neighbours (SP f x) = [SP f $ adjust not i x | i <- keys x]

-- The formulas are equal, so we only need one
crossoverSAT :: SATCrossOver -> SATProblem -> SATProblem -> SATProblem
crossoverSAT co (SP f s1) (SP _ s2) = SP f . fromList $
    case co of
      (LeftOf x)  -> clsat x (M.toList s1) (M.toList s2)
      (RightOf x) -> crsat x (M.toList s1) (M.toList s2)
  where
    clsat 0 s1 s2 = s1
    clsat n (_:s1) (y:s2) = y : clsat (n - 1) s1 s2
    crsat 0 [] [] = []
    crsat 0 (_:s1) (y:s2) = y : crsat 0 s1 s2
    crsat n (x:s1) (_:s2) = x : crsat (n - 1) s1 s2
  
-- Parser; we should split this file somehow
readCNF :: FilePath -> IO (Either ParseError SAT)
readCNF x = readFile x >>= return . runParser pCNFFile () x

pCNFFile :: Parsec String () SAT
pCNFFile = do
  many pCommentLine
  (vcount, ccount) <- pProblemLine
  spaces
  cs <- pClauses ccount vcount
  -- TODO better error handling?
  return $ SAT cs

pCommentLine :: Parsec String () String
pCommentLine = char 'c' *> (option "" $ char ' ' *> many (noneOf "\n")) <* char '\n'

pProblemLine :: Parsec String () (Int, Int)
pProblemLine = (,) <$
      char 'p'
  <*  pWhitespace
  <*  string "cnf"
  <*  pWhitespace
  <*> pInt
  <*  pWhitespace
  <*> pInt
  <*  many (satisfy $ \x -> isSpace x && x /= '\n')
  <*  char '\n'

pInt :: Parsec String () Int
pInt = read <$> many1 digit

pClauses :: Int -- ^ The amount of clauses
         -> Int -- ^ The amount of variables (for error handling)
         -> Parsec String () [Clause] -- ^ The clauses in the input
pClauses cc vc = fmap Clause <$> sequence (fmap (const pNonLastClause) [1..cc-1] ++ [pLastClause])

pNonLastClause :: Parsec String () [Variable]
pNonLastClause = (:) <$> pNotVar <*> pNonLastClause <|> do
  x <- digit
  if x == '0'
    then pWhitespace *> return []
    else do
      xs <- many digit
      pWhitespace
      rest <- pNonLastClause
      return $ Var (x:xs) : rest

pLastClause :: Parsec String () [Variable]
pLastClause = many $ pNotVar <|> pVar

pVar :: Parsec String () Variable
pVar = Var <$> many digit <* pWhitespace

pNotVar :: Parsec String () Variable
pNotVar = Not <$ char '-' <*> many digit <* pWhitespace

pWhitespace :: Parsec String () ()
pWhitespace = () <$ space <* spaces

-- intercalate " & " (show <$> x)
pSAT :: Parsec String () SAT
pSAT = SAT <$> sepBy pClause (string " & ")

-- "(" ++ intercalate "|" (show <$> x) ++ ")"
pClause :: Parsec String () Clause
pClause = Clause <$ char '(' <*> sepBy pVariable (char '|') <* char ')'

{-
instance Show Variable where
  show (Not x) = "-" ++ x
  show (Var x) = x
-}
pVariable :: Parsec String () Variable
pVariable =
      Not <$  char '-' <*> many letter
  <|> Var <$>              many letter

  
