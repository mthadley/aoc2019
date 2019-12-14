#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Data.Foldable (maximumBy)
import Data.List (groupBy, sortBy, transpose)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as S

type Point = (Int, Int)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

mapWithPosition :: (Point -> a -> b) -> [[a]] -> [b]
mapWithPosition f = concat . mapWithIndex (\y -> mapWithIndex (\x -> f (x, y)))

getAngle :: (Int, Int) -> (Int, Int) -> Float
getAngle (x1, y1) (x2, y2) =
  let angle =
        atan2
          (fromIntegral y2 - fromIntegral y1)
          (fromIntegral x2 - fromIntegral x1)
   in if angle < atan2 (-1) 0
        then angle + pi * 2
        else angle

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

part1 :: [Point] -> Int
part1 asteroidPoints =
  let countRoids p1 = length $ S.fromList $ getAngle p1 <$> asteroidPoints
   in maximum $ countRoids <$> asteroidPoints

part2 :: [Point] -> Int
part2 asteroidPoints =
  let countRoids p1 = length $ S.fromList $ getAngle p1 <$> asteroidPoints
      stationPosition = maximumBy (comparing countRoids) asteroidPoints
      pairsSortedByRotation =
        concat $
        transpose $
        fmap (sortBy (comparing (distance stationPosition . fst))) <$>
        groupBy (\(_, a1) (_, a2) -> a1 == a2) $
        sortBy (comparing snd) $
        (\p -> (p, getAngle stationPosition p)) <$>
        filter (/= stationPosition) asteroidPoints
      (x, y) = fst $ pairsSortedByRotation !! 199
   in x * 100 + y

main :: IO ()
main = do
  asteroidMap <- lines <$> readFile "Day10-input.txt"
  let asteroidPoints =
        catMaybes $
        mapWithPosition
          (\p space ->
             if space == '#'
               then Just p
               else Nothing)
          asteroidMap
  putStrLn $ "Part 1: " <> (show $ part1 asteroidPoints)
  putStrLn $ "Part 2: " <> (show $ part2 asteroidPoints)
