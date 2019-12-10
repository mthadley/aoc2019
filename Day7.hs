#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

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

data Program
  = PausedAtOutput Int
                   (Int, S.Seq Int) -- (PC, Codes)
  | Unstarted (S.Seq Int)
  | Halted
  | BadCode Int

isHalted :: Program -> Bool
isHalted program =
  case program of
    Halted -> True
    _ -> False

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

run :: [Int] -> Program -> Program
run originalInputs program =
  let runHelp pc inputs codes =
        let readWithMode mode pos =
              let val = S.index codes pos
               in case mode of
                    Position -> S.index codes val
                    Immediate -> val
            runWith f aMode bMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = S.index codes $ pc + 3
               in runHelp (pc + 4) inputs $
                  S.update output (f inputA inputB) codes
            jumpWith f aMode jMode =
              let val = readWithMode aMode (pc + 1)
                  nextPC =
                    if f val
                      then readWithMode jMode (pc + 2)
                      else pc + 3
               in runHelp nextPC inputs codes
            compareWith f aMode bMode =
              let val =
                    if (readWithMode aMode (pc + 1) `f`
                        readWithMode bMode (pc + 2))
                      then 1
                      else 0
                  addr = S.index codes $ pc + 3
               in runHelp (pc + 4) inputs $ S.update addr val codes
         in case decodeInstruction $ S.index codes pc of
              Right (Add aMode bMode) -> runWith (+) aMode bMode
              Right (Multiply aMode bMode) -> runWith (*) aMode bMode
              Right Halt -> Halted
              Right Save ->
                let (input, newInputs) =
                      case inputs of
                        x:xs -> (x, xs)
                        [] -> (0, [])
                    addr = S.index codes $ pc + 1
                 in runHelp (pc + 2) newInputs $ S.update addr input codes
              Right (Output oMode) -> do
                let output = readWithMode oMode (pc + 1)
                 in PausedAtOutput output (pc + 2, codes)
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode) -> compareWith (<) aMode bMode
              Right (Equals aMode bMode) -> compareWith (==) aMode bMode
              Left badCode -> BadCode badCode
   in case program of
        PausedAtOutput _ (pc, codes) -> runHelp pc originalInputs codes
        Unstarted codes -> runHelp 0 originalInputs codes
        Halted -> Halted
        BadCode code -> BadCode code

part1 :: S.Seq Int -> Int
part1 code =
  let runAmplifiers program =
        let runAmp output setting =
              case run [setting, output] program of
                Halted -> -1
                BadCode _ -> -1
                PausedAtOutput value _ -> value
                Unstarted _ -> -1
         in foldl runAmp 0
   in maximum $ runAmplifiers (Unstarted code) <$> permutations [0 .. 4]

part2 :: S.Seq Int -> Int
part2 codes =
  let runAmp output (setting, amp) =
        let result = run (catMaybes [setting, output]) amp
         in case result of
              Halted -> (output, (Nothing, result))
              Unstarted _ -> (output, (Nothing, result))
              BadCode _ -> (output, (Nothing, result))
              PausedAtOutput value _ -> (Just value, (Nothing, result))
      runCycle output amps =
        let (lastOutput, newAmps) = mapAccumL runAmp output amps
         in if all (isHalted . snd) newAmps
              then lastOutput
              else runCycle lastOutput newAmps
      thrusterSignal settings =
        runCycle Nothing $ zip (Just <$> settings) (repeat $ Unstarted codes)
   in maximum $ catMaybes $ thrusterSignal <$> permutations [5 .. 9]

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
