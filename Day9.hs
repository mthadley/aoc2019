#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read (decimal, signed)
import Protolude

data PMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data Instruction
  = Add PMode
        PMode
        PMode
  | Multiply PMode
             PMode
             PMode
  | Save PMode
  | Output PMode
  | JumpIfTrue PMode
               PMode
  | JumpIfFalse PMode
                PMode
  | LessThan PMode
             PMode
             PMode
  | Equals PMode
           PMode
           PMode
  | UpdateBase PMode
  | Halt
  deriving (Show)

data Program
  = PausedAtOutput Integer
                   Machine
  | Unstarted Machine
  | Halted Reason
  deriving (Show)

data Machine = Machine
  { pc_ :: Integer
  , base_ :: Integer
  , inputs_ :: [Integer]
  , codes_ :: M.Map Integer Integer
  } deriving (Show)

data Reason
  = Done
  | BadCode Integer
  deriving (Show)

fromCodes :: [Integer] -> Program
fromCodes = Unstarted . Machine 0 0 [] . M.fromList . zipWith (,) [0 ..]

isHalted :: Program -> Bool
isHalted program =
  case program of
    Halted _ -> True
    _ -> False

decodeInstruction :: Integer -> Either Integer Instruction
decodeInstruction i =
  let mode :: Integer -> PMode
      mode p =
        case i `div` (10 ^ (p + 1)) `mod` 10 of
          0 -> Position
          1 -> Immediate
          _ -> Relative -- Mode 2
   in case i `mod` 100 of
        1 -> Right $ Add (mode 1) (mode 2) (mode 3)
        2 -> Right $ Multiply (mode 1) (mode 2) (mode 3)
        3 -> Right $ Save (mode 1)
        4 -> Right $ Output (mode 1)
        5 -> Right $ JumpIfTrue (mode 1) (mode 2)
        6 -> Right $ JumpIfFalse (mode 1) (mode 2)
        7 -> Right $ LessThan (mode 1) (mode 2) (mode 3)
        8 -> Right $ Equals (mode 1) (mode 2) (mode 3)
        9 -> Right $ UpdateBase (mode 1)
        99 -> Right Halt
        badCode -> Left badCode

run :: [Integer] -> Program -> Program
run originalInputs program =
  let runHelp machine@(Machine pc base inputs codes) =
        let readMem pos = fromMaybe 0 $ M.lookup pos codes
            readWithMode mode pos =
              let val = readMem pos
               in case mode of
                    Position -> readMem val
                    Immediate -> val
                    Relative -> readMem (base + val)
            runWith f aMode bMode oMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = readWithMode oMode $ pc + 3
               in runHelp $
                  machine
                    { pc_ = pc + 4
                    , codes_ = M.insert output (f inputA inputB) codes
                    }
            jumpWith f aMode jMode =
              let val = readWithMode aMode (pc + 1)
                  nextPC =
                    if f val
                      then readWithMode jMode (pc + 2)
                      else pc + 3
               in runHelp $ machine {pc_ = nextPC}
            compareWith f aMode bMode oMode =
              let val =
                    if (readWithMode aMode (pc + 1) `f`
                        readWithMode bMode (pc + 2))
                      then 1
                      else 0
                  addr = readWithMode oMode $ pc + 3
               in runHelp $
                  machine {pc_ = pc + 4, codes_ = M.insert addr val codes}
         in case decodeInstruction (readMem pc) of
              Right (Add aMode bMode oMode) -> runWith (+) aMode bMode oMode
              Right (Multiply aMode bMode oMode) ->
                runWith (*) aMode bMode oMode
              Right Halt -> Halted Done
              Right (Save oMode) ->
                let (input, newInputs) =
                      case inputs of
                        x:xs -> (x, xs)
                        [] -> (0, [])
                    addr
                      -- https://www.reddit.com/r/adventofcode/comments/e8aw9j/2019_day_9_part_1_how_to_fix_203_error/
                     =
                      case oMode of
                        Position -> readMem $ pc + 1
                        Immediate -> readMem $ pc + 1
                        Relative -> base + (readMem $ pc + 1)
                 in runHelp $
                    machine
                      { inputs_ = newInputs
                      , pc_ = pc + 2
                      , codes_ = M.insert addr input codes
                      }
              Right (Output oMode) -> do
                let output = readWithMode oMode (pc + 1)
                 in PausedAtOutput output $ machine {pc_ = pc + 2}
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode oMode) ->
                compareWith (<) aMode bMode oMode
              Right (Equals aMode bMode oMode) ->
                compareWith (==) aMode bMode oMode
              Right (UpdateBase aMode) ->
                let newBase = base + (readWithMode aMode $ pc + 1)
                 in runHelp $ machine {pc_ = pc + 2, base_ = newBase}
              Left badCode -> Halted $ BadCode badCode
   in case program of
        PausedAtOutput _ machine ->
          runHelp $ machine {inputs_ = inputs_ machine <> originalInputs}
        Unstarted machine ->
          runHelp $ machine {inputs_ = inputs_ machine <> originalInputs}
        Halted reason -> Halted reason

runToCompletion :: [Integer] -> Program -> [Integer]
runToCompletion originalInputs =
  let runHelp inputs outputsSoFar program =
        let newProgram = run inputs program
         in case newProgram of
              PausedAtOutput value _ ->
                runHelp [] (outputsSoFar ++ [value]) newProgram
              Unstarted _ -> runHelp [] outputsSoFar newProgram
              Halted _ -> outputsSoFar
   in runHelp originalInputs []

part1 :: [Integer] -> [Integer]
part1 code = runToCompletion [23] (fromCodes code)

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    traverse readCode <$> T.splitOn "," <$> readFile "Day9-input.txt"
  case parsedCodes of
    Left _ -> putText "Failed to parse codes."
    Right codes -> do
      putText "Part 1:"
      print $ part1 codes
      print $ part1 [109, -1, 4, 1, 99] == [-1]
      print $ part1 [109, -1, 4, 1, 99] == [-1]
      print $ part1 [109, -1, 104, 1, 99] == [1]
      print $ part1 [109, -1, 204, 1, 99] == [109]
      print $ part1 [109, 1, 9, 2, 204, -6, 99] == [204]
      print $ part1 [109, 1, 109, 9, 204, -6, 99] == [204]
      print $ part1 [109, 1, 209, -1, 204, -106, 99] == [204]
      print $ part1 [109, 1, 3, 3, 204, 2, 99] == [23]
      print $ part1 [109, 1, 203, 2, 204, 2, 99] == [23]
