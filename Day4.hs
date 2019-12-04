#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Data.List (group)

hasAnyGroup :: String -> Bool
hasAnyGroup = any ((<=) 2 . length) . group

hasDoubleGroup :: String -> Bool
hasDoubleGroup = any ((==) 2 . length) . group

allIncrease :: String -> Bool
allIncrease password = all id $ zipWith (<=) password (drop 1 password)

main :: IO ()
main = do
  let input = show <$> ([137638 .. 596253] :: [Int])
  let validPart1 password = hasAnyGroup password && allIncrease password
  putStrLn $ "Part 1: " <> (show $ length $ filter validPart1 input)
  let validPart2 password = hasDoubleGroup password && allIncrease password
  putStrLn $ "Part 2: " <> (show $ length $ filter validPart2 input)
