#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}

import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text.Read (decimal, signed)
import Protolude

data PMode
  = Position
  | Immediate
  | Relative

data WMode
  = WPosition
  | WRelative

data Instruction
  = Add PMode
        PMode
        WMode
  | Multiply PMode
             PMode
             WMode
  | Save WMode
  | Output PMode
  | JumpIfTrue PMode
               PMode
  | JumpIfFalse PMode
                PMode
  | LessThan PMode
             PMode
             WMode
  | Equals PMode
           PMode
           WMode
  | UpdateBase PMode
  | Halt

data Program
  = PausedAtOutput Integer
                   Machine
  | Unstarted Machine
  | Halted Reason

data Machine = Machine
  { pc_ :: Integer
  , base_ :: Integer
  , inputs_ :: [Integer]
  , codes_ :: M.Map Integer Integer
  }

data Reason
  = Done
  | BadCode Integer

fromCodes :: [Integer] -> Program
fromCodes = Unstarted . Machine 0 0 [] . M.fromList . zipWith (,) [0 ..]

isHalted :: Program -> Bool
isHalted program =
  case program of
    Halted _ -> True
    _ -> False

decodeInstruction :: Integer -> Either Integer Instruction
decodeInstruction i =
  let modeCode p = i `div` (10 ^ (p + 1)) `mod` 10
      mode :: Integer -> PMode
      mode p =
        case modeCode p of
          1 -> Immediate
          2 -> Relative
          _ -> Position -- Mode 0
      wmode :: Integer -> WMode
      wmode p =
        case modeCode p of
          2 -> WRelative
          _ -> WPosition -- Mode 0
   in case i `mod` 100 of
        1 -> Right $ Add (mode 1) (mode 2) (wmode 3)
        2 -> Right $ Multiply (mode 1) (mode 2) (wmode 3)
        3 -> Right $ Save (wmode 1)
        4 -> Right $ Output (mode 1)
        5 -> Right $ JumpIfTrue (mode 1) (mode 2)
        6 -> Right $ JumpIfFalse (mode 1) (mode 2)
        7 -> Right $ LessThan (mode 1) (mode 2) (wmode 3)
        8 -> Right $ Equals (mode 1) (mode 2) (wmode 3)
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
            readWithModeForWrite oMode pos =
              let val = readMem pos
               in case oMode of
                    WPosition -> val
                    WRelative -> base + val
            runWith f aMode bMode oMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = readWithModeForWrite oMode $ pc + 3
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
                  addr = readWithModeForWrite oMode $ pc + 3
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
                    addr = readWithModeForWrite oMode $ pc + 1
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

type Point = (Int, Int)

data Color
  = White
  | Black

colorToInput :: Color -> Integer
colorToInput =
  \case
    White -> 1
    Black -> 0

colorFromOutput :: Integer -> Color
colorFromOutput =
  \case
    1 -> White
    0 -> Black
    _ -> Black

data Direction
  = North
  | South
  | East
  | West

turnRight :: Direction -> Direction
turnRight =
  \case
    North -> East
    East -> South
    South -> West
    West -> North

turnLeft :: Direction -> Direction
turnLeft =
  \case
    North -> West
    West -> South
    South -> East
    East -> North

move :: Direction -> Point -> Point
move direction (x, y) =
  case direction of
    North -> (x, y + 1)
    East -> (x + 1, y)
    South -> (x, y - 1)
    West -> (x - 1, y)

data Simulation = Simulation
  { program_ :: Program
  , squares_ :: Map Point Color
  , position_ :: Point
  , direction_ :: Direction
  , lastOutput_ :: Maybe Integer
  }

getPaintedPanels :: Color -> [Integer] -> Map Point Color
getPaintedPanels startingColor code =
  let step simulation@(Simulation program squares position direction lastOutput) =
        let lookupColor pos = fromMaybe Black $ M.lookup pos squares
         in case (program, lastOutput) of
              (Unstarted _, _) ->
                step $
                simulation {program_ = run [colorToInput startingColor] program}
              (PausedAtOutput output _, Nothing) ->
                step $
                simulation
                  {program_ = run [] program, lastOutput_ = Just output}
              (PausedAtOutput nextDirectionCode _, Just nextColorCode) ->
                let nextDirection =
                      if nextDirectionCode == 0
                        then turnLeft direction
                        else turnRight direction
                    nextPosition = move nextDirection position
                 in step $
                    simulation
                      { program_ =
                          run [colorToInput $ lookupColor nextPosition] program
                      , squares_ =
                          M.insert
                            position
                            (colorFromOutput nextColorCode)
                            squares
                      , position_ = nextPosition
                      , direction_ = nextDirection
                      , lastOutput_ = Nothing
                      }
              (Halted _, _) -> squares
   in step $
      Simulation
        { program_ = fromCodes code
        , squares_ = M.empty
        , position_ = (0, 0)
        , direction_ = North
        , lastOutput_ = Nothing
        }

printPaintedPanels :: Map Point Color -> IO ()
printPaintedPanels squares =
  let points = M.keys squares
      minX = minimum $ fst <$> points
      minY = minimum $ snd <$> points
      maxX = maximum $ fst <$> points
      maxY = maximum $ snd <$> points
      colorToChar (x, y) =
        case fromMaybe Black $ M.lookup (x, y) squares of
          White -> 'X'
          Black -> ' '
      prettyLines =
        fmap
          (\y -> fmap (\x -> colorToChar (x, y)) [minX .. maxX])
          [maxY,maxY - 1 .. minY]
   in putText $ T.unlines $ toS <$> prettyLines

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    traverse readCode <$> T.splitOn "," <$> readFile "Day11-input.txt"
  case parsedCodes of
    Left _ -> putText "Failed to parse codes."
    Right codes -> do
      putText $ "Part 1: " <> (show $ M.size $ getPaintedPanels Black codes)
      putText $ "Part 2: \n"
      printPaintedPanels $ getPaintedPanels White codes
