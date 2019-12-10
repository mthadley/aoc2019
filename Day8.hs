#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Data.Foldable (minimumBy)
import Data.List.Split (chunksOf)

part1 :: [String] -> Int
part1 layers =
  let count num = length . filter ((==) num)
      leastZeros = minimumBy (\a b -> count '0' a `compare` count '0' b) layers
   in count '1' leastZeros * count '2' leastZeros

part2 :: [String] -> IO ()
part2 layers = do
  let combineLayers '2' b = b
      combineLayers a _ = a
      render '0' = ' '
      render _ = 'X'
  putStrLn $
    unlines $
    chunksOf 25 $ render <$> foldl (zipWith combineLayers) (repeat '2') layers

main :: IO ()
main = do
  layers <-
    chunksOf (25 * 6) <$> filter ((/=) '\n') <$> readFile "Day8-input.txt"
  putStrLn $ "Part 1: " <> (show $ part1 layers)
  putStrLn "Part 2: "
  part2 layers
