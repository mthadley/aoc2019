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
  | Relative

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
  | UpdateBase
  | Halt

data Program
  = PausedAtOutput Int
                   Machine
  | Unstarted Machine
  | Halted Reason

data Machine = Machine
  { pc_ :: Int
  , base_ :: Int
  , codes_ :: S.Seq Int -- TODO: Switch to a map. Data.HashMap?
  }

fromCodes :: S.Seq Int -> Program
fromCodes = Unstarted . Machine 0 0

data Reason
  = Done
  | BadCode Int

isHalted :: Program -> Bool
isHalted program =
  case program of
    Halted _ -> True
    _ -> False

decodeInstruction :: Int -> Either Int Instruction
decodeInstruction i =
  let mode :: Int -> PMode
      mode p =
        case i `div` (10 ^ (p + 1)) `mod` 10 of
          0 -> Position
          1 -> Immediate
          _ -> Relative -- Mode 2
   in case i `mod` 100 of
        1 -> Right $ Add (mode 1) (mode 2)
        2 -> Right $ Multiply (mode 1) (mode 2)
        3 -> Right $ Save
        4 -> Right $ Output (mode 1)
        5 -> Right $ JumpIfTrue (mode 1) (mode 2)
        6 -> Right $ JumpIfFalse (mode 1) (mode 2)
        7 -> Right $ LessThan (mode 1) (mode 2)
        8 -> Right $ Equals (mode 1) (mode 2)
        9 -> Right $ UpdateBase
        99 -> Right Halt
        badCode -> Left badCode

run :: [Int] -> Program -> Program
run originalInputs program =
  let runHelp inputs machine@(Machine pc base codes) =
        let readWithMode mode pos =
              let val = S.index codes pos
               in case mode of
                    Position -> S.index codes val
                    Immediate -> val
                    Relative -> S.index codes (val + base)
            runWith f aMode bMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = S.index codes $ pc + 3
               in runHelp inputs $
                  machine
                    { pc_ = pc + 4
                    , codes_ = S.update output (f inputA inputB) codes
                    }
            jumpWith f aMode jMode =
              let val = readWithMode aMode (pc + 1)
                  nextPC =
                    if f val
                      then readWithMode jMode (pc + 2)
                      else pc + 3
               in runHelp inputs $ machine {pc_ = nextPC}
            compareWith f aMode bMode =
              let val =
                    if (readWithMode aMode (pc + 1) `f`
                        readWithMode bMode (pc + 2))
                      then 1
                      else 0
                  addr = S.index codes $ pc + 3
               in runHelp inputs $
                  machine {pc_ = pc + 4, codes_ = S.update addr val codes}
         in case decodeInstruction $ S.index codes pc of
              Right (Add aMode bMode) -> runWith (+) aMode bMode
              Right (Multiply aMode bMode) -> runWith (*) aMode bMode
              Right Halt -> Halted Done
              Right Save ->
                let (input, newInputs) =
                      case inputs of
                        x:xs -> (x, xs)
                        [] -> (0, [])
                    addr = S.index codes $ pc + 1
                 in runHelp newInputs $
                    machine {pc_ = pc + 2, codes_ = S.update addr input codes}
              Right (Output oMode) -> do
                let output = readWithMode oMode (pc + 1)
                 in PausedAtOutput output $ machine {pc_ = pc + 2}
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode) -> compareWith (<) aMode bMode
              Right (Equals aMode bMode) -> compareWith (==) aMode bMode
              Right UpdateBase ->
                runHelp inputs $
                machine {pc_ = pc + 2, base_ = base + (S.index codes $ pc + 1)}
              Left badCode -> Halted $ BadCode badCode
   in case program of
        PausedAtOutput _ machine -> runHelp originalInputs machine
        Unstarted machine -> runHelp originalInputs machine
        Halted reason -> Halted reason

runToCompletion :: [Int] -> Program -> [Int]
runToCompletion inputs =
  let runHelp outputsSoFar program =
        case run inputs program of
          PausedAtOutput value _ -> runHelp (outputsSoFar ++ [value]) program
          Unstarted _ -> runHelp outputsSoFar program
          Halted _ -> outputsSoFar
   in runHelp []

part1 :: S.Seq Int -> IO ()
part1 code = print $ runToCompletion [1] (fromCodes code)

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    fmap S.fromList <$> traverse readCode <$> T.splitOn "," <$>
    readFile "Day9-input.txt"
  case parsedCodes of
    Left _ -> putText "Failed to parse codes."
    Right codes -> do
      putText $ "Part 1: "
      part1 codes
