{-# LANGUAGE RankNTypes #-}
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

data TSP = TSP
  { name  :: String
  , dimension :: Int
  , nodes :: [Node]
  }
  deriving (Show)

type Node = Point Float
type Point a = (a, a)

type Fields = [Field]
type Field  = (String, String)
type DistFunc a = Num a => Point a -> Point a -> a

-- * Solver

instance Searchable TSP where
  neighbours p = [p { nodes = n } | n <- opt2 $ nodes p]
  score = negate . distance euclidean . nodes

shuffle :: TSP -> IO TSP
shuffle p = do
  let n = nodes p
  n' <- shuffleM n
  return $ p { nodes = n' }  

distance :: Num a => DistFunc a -> [Point a] -> a
distance f (x:y:r) = f x y + distance f (y:r)
distance f _       = 0

euclidean :: Floating a => DistFunc a
euclidean (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

opt2 :: [a] -> [[a]]
opt2 xs = [ reverseRange i j xs | i <- [1..length xs], j <- [0..i-1]]

reverseRange :: Int -> Int -> [a] -> [a]
reverseRange i j xs 
  | i <= j     = h ++ reverse m ++ t
  | otherwise = reverseRange j i xs
    where
      (h, r) = splitAt i xs
      (m, t) = splitAt (j - i) r


-- * Parsing

readTSP :: FilePath -> IO (Either ParseError TSP)
readTSP x = runParser pTSP () x <$> readFile x 

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

pInt :: Parsec String () Float
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

