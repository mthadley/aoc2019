#!/usr/bin/env stack
{- stack script --resolver lts-14.15 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import qualified Data.Map.Lazy as M
import Protolude
import Text.Parsec as P
import Text.Parsec.Char as PC
import Text.Parsec.Text (Parser, parseFromFile)

type Planet = Text

type OrbitMap = M.Map Planet Planet

parsePairs :: Parser [(Planet, Planet)]
parsePairs =
  let code = toS <$> P.many1 PC.alphaNum
      pair = flip (,) <$> code <* PC.char ')' <*> code
   in P.sepEndBy pair PC.newline

foldTo :: (Monoid a) => (Planet -> a) -> OrbitMap -> Planet -> Planet -> a
foldTo f orbitMap target start =
  let jump soFar key =
        case M.lookup key orbitMap of
          Just planet ->
            if target == planet
              then soFar
              else jump (f planet <> soFar) planet
          Nothing -> mempty
   in jump mempty start

countJumps :: OrbitMap -> Planet -> Planet -> Int
countJumps orbitMap target =
  (+) 1 . getSum . foldTo (const $ Sum 1) orbitMap target

part1 :: OrbitMap -> Int
part1 orbitMap = sum $ fmap (countJumps orbitMap "COM") $ M.keys orbitMap

part2 :: OrbitMap -> Int
part2 orbitMap =
  let planetsToRoot = foldTo pure orbitMap "COM"
      inCommon a b =
        lastMay $
        takeWhile (\(p1, p2) -> p1 == p2) $
        zip (planetsToRoot a) (planetsToRoot b)
   in case inCommon "YOU" "SAN" of
        Just (common, _) ->
          countJumps orbitMap common "YOU" + countJumps orbitMap common "SAN" -
          2 -- Minus you and santa
        Nothing -> -1

main :: IO ()
main = do
  parsed <- fmap M.fromList <$> parseFromFile parsePairs "Day6-input.txt"
  case parsed of
    Left _ -> putText "Failed to parse input!"
    Right orbitMap -> do
      putText $ "Part 1: " <> (show $ part1 orbitMap)
      putText $ "Part 2: " <> (show $ part2 orbitMap)
