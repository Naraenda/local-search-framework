module LocalSearch.Tests.Problems.TSP
  ( TSP(..)
  , Node, Point, Fields, Field
  , readTSP
  )
where

import Data.Char (isLetter, isSpace)
import Data.Maybe (fromJust)

import Text.Parsec

data TSP = TSP
  { name  :: String
  , dimension :: Int
  , nodes :: [Node]
  }
  deriving (Show)

type Node = Point Int
type Point a = (a, a)

type Fields = [Field]
type Field  = (String, String)

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

pInt :: Parsec String () Int
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

