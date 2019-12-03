#!/usr/bin/env stack
{- stack script --resolver lts-14.15 --ghc-options -Wall -}
import Control.Monad (fail)
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.Parsec.Char as PC
import Text.Parsec.Text (Parser, parseFromFile)

data Direction
  = North
  | South
  | East
  | West

data Segment =
  Segment Direction
          Int

type Wire = [Segment]

parseWires :: Parser [Wire]
parseWires =
  let parseLength = read <$> P.many1 PC.digit
      parseDirection = do
        c <- PC.letter
        case c of
          'U' -> pure North
          'D' -> pure South
          'L' -> pure West
          'R' -> pure East
          _ -> fail "Invalid direction"
      parseSegment = Segment <$> parseDirection <*> parseLength
   in P.sepEndBy (P.sepBy1 parseSegment (PC.char ',')) PC.newline

type Point = (Int, Int)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

vector :: Direction -> Int -> Point
vector dir m =
  case dir of
    North -> (0, 1 * m)
    South -> (0, -1 * m)
    West -> (-1 * m, 0)
    East -> (1 * m, 0)

points :: Wire -> [Point]
points =
  let addPoints (pointsSoFar, pos) (Segment dir len) =
        let newPoints = (add pos . vector dir) <$> [1 .. len]
         in (pointsSoFar ++ newPoints, add pos (vector dir len))
   in fst . foldl addPoints ([], (0, 0))

part1 :: [Wire] -> Int
part1 =
  minimum .
  S.map (distance (0, 0)) . foldl1 S.intersection . fmap (S.fromList . points)

segmentsTo :: Point -> [Point] -> Int
segmentsTo point = (+) 1 . length . takeWhile ((/= point))

part2 :: [Wire] -> Int
part2 wires =
  let wirePoints = fmap points wires
      inters = foldl1 S.intersection $ S.fromList <$> wirePoints
   in minimum $ S.map (\inter -> sum $ segmentsTo inter <$> wirePoints) inters

main :: IO ()
main = do
  result <- parseFromFile parseWires "Day3-input.txt"
  case result of
    Left _ -> print "Failed to parse input."
    Right wires -> do
      putStrLn $ "Part 1: " <> (show $ part1 wires)
      putStrLn $ "Part 2: " <> (show $ part2 wires)
