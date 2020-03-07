module LocalSearch.Tests.Problems.Satisfyability 
  ( SATProblem(..)
  , SAT(..)
  , Clause(..)
  , Variable(..)
  , Solution(..)
  , Evaluable(..)

  , readCNF
  )
  where

import Data.Char (isSpace)
import Data.List(intercalate)
import Data.Set(Set, unions, singleton)
import Data.Map(Map, (!), adjust, keys)
import Data.Maybe (catMaybes)

import Test.QuickCheck

import Text.Parsec

import LocalSearch.Framework.SearchProblem

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

instance Searchable SATProblem where
  score (SP f x) = fromIntegral . snd $ eval x f
  neighbours (SP f x) = [SP f $ adjust not i x | i <- keys x]

-- Parser; we should split this file somehow
readCNF :: FilePath -> IO (Either ParseError SAT)
readCNF x = readFile x >>= return . runParser pCNFFile () x

pCNFFile :: Parsec String () SAT
pCNFFile = do
  many pCommentLine
  (vcount, ccount) <- pProblemLine
  pWhitespace
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

