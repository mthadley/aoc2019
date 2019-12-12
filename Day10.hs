#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Data.Maybe (catMaybes)
import qualified Data.Set as S

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

mapWithPosition :: ((Int, Int) -> a -> b) -> [[a]] -> [b]
mapWithPosition f = concat . mapWithIndex (\y -> mapWithIndex (\x -> f (x, y)))

getAngle :: (Int, Int) -> (Int, Int) -> Float
getAngle (x1, y1) (x2, y2) =
  atan2 (fromIntegral y2 - fromIntegral y1) (fromIntegral x2 - fromIntegral x1)

part1 :: [[Char]] -> Int
part1 asteroidMap =
  let asteroidPoints =
        catMaybes $
        mapWithPosition
          (\p space ->
             if space == '#'
               then Just p
               else Nothing)
          asteroidMap
      countRoids p1 = length $ S.fromList $ getAngle p1 <$> asteroidPoints
   in maximum $ countRoids <$> asteroidPoints

main :: IO ()
main = do
  asteroidMap <- lines <$> readFile "Day10-input.txt"
  putStrLn $ "Part 1: " <> (show $ part1 asteroidMap)
