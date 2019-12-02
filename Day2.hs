#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Protolude

run :: S.Seq Int -> S.Seq Int
run =
  let runHelp pc codes =
        let runWith f =
              let inputA = S.index codes $ S.index codes $ pc + 1
                  inputB = S.index codes $ S.index codes $ pc + 2
                  output = S.index codes $ pc + 3
               in runHelp (pc + 4) (S.update output (f inputA inputB) codes)
         in case S.index codes pc of
              1 -> runWith (+)
              2 -> runWith (*)
          -- Halt
              99 -> codes
          -- Just bail out for unknown codes
              _ -> codes
   in runHelp 0

partTwo :: S.Seq Int -> Int
partTwo codes =
  let results = do
        noun <- [0 .. 99]
        verb <- [0 .. 99]
        pure (noun, verb, run $ S.update 1 noun $ S.update 2 verb codes)
   in case find (\(_, _, s) -> S.index s 0 == 19690720) results of
        Just (noun, verb, _) -> 100 * noun + verb
        Nothing -> -1

main :: IO ()
main = do
  let readCode t = fst <$> decimal t
  parsedCodes <-
    fmap S.fromList <$> traverse readCode <$> T.splitOn "," <$>
    readFile "Day2-input.txt"
  case parsedCodes of
    Left _ -> putLText "Failed to parse codes."
    Right codes -> do
      let codesBeforeFire = S.update 1 12 $ S.update 2 2 codes
      putText $ "Part 1: " <> (T.pack $ show $ S.index (run codesBeforeFire) 0)
      putText $ "Part 1: " <> (T.pack $ show $ partTwo codes)
