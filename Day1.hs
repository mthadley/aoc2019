#!/usr/bin/env stack
{- stack script
    --resolver lts-14.16
    --ghc-options -Wall
-}
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  masses <- mapMaybe readMaybe <$> lines <$> readFile "Day1-input.txt"
  let calculateWith f = show $ sum $ f <$> masses
  putStrLn $ "Part 1: " <> calculateWith fuelRequired
  putStrLn $ "Part 2: " <> calculateWith fuelRequiredPartTwo

fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2

fuelRequiredPartTwo :: Integer -> Integer
fuelRequiredPartTwo mass =
  let fuel = mass `div` 3 - 2
   in if fuel <= 0
        then 0
        else fuel + fuelRequiredPartTwo fuel
