#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Data.List (group)

hasGroupWithCount :: (Eq a) => (Int -> Bool) -> [a] -> Bool
hasGroupWithCount f = any (f . length) . group

allIncrease :: (Ord a) => [a] -> Bool
allIncrease password = all id $ zipWith (<=) password (drop 1 password)

main :: IO ()
main = do
  let input = show <$> ([137638 .. 596253] :: [Int])
  let validPart1 p = hasGroupWithCount ((<=) 2) p && allIncrease p
  putStrLn $ "Part 1: " <> (show $ length $ filter validPart1 input)
  let validPart2 p = hasGroupWithCount ((==) 2) p && allIncrease p
  putStrLn $ "Part 2: " <> (show $ length $ filter validPart2 input)
