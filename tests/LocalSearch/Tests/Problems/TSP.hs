{-# LANGUAGE RankNTypes
  , MultiParamTypeClasses
  , TypeFamilies #-}
module LocalSearch.Tests.Problems.TSP
  ( TSP(..)
  , Node, Point, Fields, Field
  , readTSP
  , shuffle
  )
where

import Data.Char (isLetter, isSpace)
import Data.Maybe (fromJust)
import LocalSearch.Framework.SearchProblem

import Text.Parsec
import System.Random.Shuffle( shuffleM )
import Control.Monad.Random.Lazy(Rand, RandomGen, getRandomR)

-- * Solver

data TSP = TSP
  { name  :: String
  , dimension :: Int
  , nodes :: [Node]
  }
  deriving (Show)

type Node = Point Double
type Point a = (a, a)

type Fields = [Field]
type Field  = (String, String)
type DistFunc a = Num a => Point a -> Point a -> a

data TSPActions
  = Opt2 Int Int

instance Heuristic TSP where
  score = negate . tourDistance euclidean . nodes

instance Searchable TSP where
  type Action TSP = TSPActions
  neighbours p = [ Opt2 i j | i <- [1 .. length (nodes p)], j <- [0 .. i - 1]]
  explore p (Opt2 i j) = p { nodes = opt2 i j (nodes p) }

-- | Shuffles a TSP problem
shuffle :: TSP -> IO TSP
shuffle p = do
  let n = nodes p
  n' <- shuffleM n
  return $ p { nodes = n' }  

-- | Given a distance function, calculates the distance of a tour
-- such that the ends loops back to the beginning.
tourDistance :: Num a => DistFunc a -> [Point a] -> a
tourDistance f xs@(h:_) = distance f (xs ++ [h])

-- | Given a distance function, calculates the distance of a route.
distance :: Num a => DistFunc a -> [Point a] -> a
distance f (x:y:r) = f x y + distance f (y:r)
distance f _       = 0

-- | Euclidean distance
euclidean :: Floating a => DistFunc a
euclidean (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- | Reverses a part of a list.
opt2 :: Int -> Int -> [a] -> [a]
opt2 i j xs 
  | i <= j     = h ++ reverse m ++ t
  | otherwise = opt2 j i xs
    where
      (h, r) = splitAt i xs
      (m, t) = splitAt (j - i) r

-- * Parsing

-- | Loads a TSP from a file.
readTSP :: FilePath -> IO (Either ParseError TSP)
readTSP x = runParser pTSP () x <$> readFile x 

-- Auxillary parser functions; internal only

-- Note that the grammar used to parse the files is quite clear from the code
-- itself. We did not make this format ourselves, but we're using this format
-- to get benchmarking problems from other places.

pTSP :: Parsec String () TSP
pTSP =
  do
    fields <- many pField
    pNodeCoordSection
    let getField' f = fromJust . getField f
    let name =        getField' "NAME"      fields
    let dim  = read $ getField' "DIMENSION" fields
    TSP name dim <$> count dim (pNode <* char '\n')

pNodeCoordSection :: Parsec String () String
pNodeCoordSection = string "NODE_COORD_SECTION" <* char '\n'

pNode :: Parsec String () Node
pNode = (,) <$ pInt <* char ' ' <*> pInt <* char ' ' <*> pInt

pInt :: Parsec String () Double
pInt = read <$> many1 digit

getField :: String -> Fields -> Maybe String
getField _ []     = Nothing
getField x ((k,v):ys)
  | x == k    = Just v
  | otherwise = getField x ys

pField :: Parsec String () Field
pField
    =  try
    $  (,)
   <$> many1 (satisfy isLetter <|> char '_') -- Key
   <*  string " : "
   <*> many1 (noneOf "\n") -- Value
   <*  char '\n'

