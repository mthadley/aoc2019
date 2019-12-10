#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Control.Monad (fail)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Read (decimal, signed)
import Protolude

data PMode
  = Position
  | Immediate

data Instruction
  = Add PMode
        PMode
  | Multiply PMode
             PMode
  | Save
  | Output PMode
  | JumpIfTrue PMode
               PMode
  | JumpIfFalse PMode
                PMode
  | LessThan PMode
             PMode
  | Equals PMode
           PMode
  | Halt

decodeInstruction :: Int -> Either Int Instruction
decodeInstruction i =
  let mode :: Int -> PMode
      mode p =
        if i `div` (10 ^ (p + 1)) `mod` 10 > 0
          then Immediate
          else Position
   in case i `mod` 100 of
        1 -> Right $ Add (mode 1) (mode 2)
        2 -> Right $ Multiply (mode 1) (mode 2)
        3 -> Right $ Save
        4 -> Right $ Output (mode 1)
        5 -> Right $ JumpIfTrue (mode 1) (mode 2)
        6 -> Right $ JumpIfFalse (mode 1) (mode 2)
        7 -> Right $ LessThan (mode 1) (mode 2)
        8 -> Right $ Equals (mode 1) (mode 2)
        99 -> Right Halt
        badCode -> Left badCode

run :: [Int] -> S.Seq Int -> (S.Seq Int, [Int])
run originalInputs =
  let runHelp pc inputs outputs codes =
        let readWithMode mode pos =
              let val = S.index codes pos
               in case mode of
                    Position -> S.index codes val
                    Immediate -> val
            runWith f aMode bMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = S.index codes $ pc + 3
               in runHelp (pc + 4) inputs outputs $
                  S.update output (f inputA inputB) codes
            jumpWith f aMode jMode =
              let val = readWithMode aMode (pc + 1)
                  nextPC =
                    if f val
                      then readWithMode jMode (pc + 2)
                      else pc + 3
               in runHelp nextPC inputs outputs codes
            compareWith f aMode bMode =
              let val =
                    if (readWithMode aMode (pc + 1) `f`
                        readWithMode bMode (pc + 2))
                      then 1
                      else 0
                  addr = S.index codes $ pc + 3
               in runHelp (pc + 4) inputs outputs $ S.update addr val codes
         in case decodeInstruction $ S.index codes pc of
              Right (Add aMode bMode) -> runWith (+) aMode bMode
              Right (Multiply aMode bMode) -> runWith (*) aMode bMode
              Right Halt -> (codes, outputs)
              Right Save ->
                let (input, newInputs) =
                      case inputs of
                        x:xs -> (x, xs)
                        [] -> (0, [])
                    addr = S.index codes $ pc + 1
                 in runHelp (pc + 2) newInputs outputs $
                    S.update addr input codes
              Right (Output oMode) -> do
                let output = readWithMode oMode (pc + 1)
                 in runHelp (pc + 2) inputs (outputs <> [output]) codes
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode) -> compareWith (<) aMode bMode
              Right (Equals aMode bMode) -> compareWith (==) aMode bMode
              Left badCode -> fail $ "Hit a bad code: " <> (show badCode)
   in runHelp 0 originalInputs []

runAmplifiers :: S.Seq Int -> [Int] -> Int
runAmplifiers codes =
  let runAmp output setting =
        fromMaybe 0 $ lastMay $ snd $ run [setting, output] codes
   in foldl runAmp 0

part1 :: S.Seq Int -> Int
part1 code = maximum $ runAmplifiers code <$> permutations [0 .. 4]

part2 :: S.Seq Int -> Int
part2 code = maximum $ runAmplifiers code <$> permutations [0 .. 4]

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    fmap S.fromList <$> traverse readCode <$> T.splitOn "," <$>
    readFile "Day7-input.txt"
  case parsedCodes of
    Left _ -> putText "Failed to parse codes."
    Right codes -> do
      putText $ "Part 1: " <> (show $ part1 codes)
      putText $ "Part 2: " <> (show $ part2 codes)
