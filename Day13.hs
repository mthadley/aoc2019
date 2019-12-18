#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

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
  , codes_ :: Map Integer Integer
  }

data Reason
  = Done
  | BadCode Integer

fromCodes :: [Integer] -> Program
fromCodes = Unstarted . Machine 0 0 . M.fromList . zipWith (,) [0 ..]

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

run :: IO (Maybe Integer) -> Program -> IO Program
run getInput program =
  let runHelp machine@(Machine pc base codes) =
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
              Right Halt -> pure $ Halted Done
              Right (Save oMode) -> do
                input <- fromMaybe 0 <$> getInput
                let addr = readWithModeForWrite oMode $ pc + 1
                runHelp $
                  machine {pc_ = pc + 2, codes_ = M.insert addr input codes}
              Right (Output oMode) ->
                let output = readWithMode oMode (pc + 1)
                 in pure $ PausedAtOutput output $ machine {pc_ = pc + 2}
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode oMode) ->
                compareWith (<) aMode bMode oMode
              Right (Equals aMode bMode oMode) ->
                compareWith (==) aMode bMode oMode
              Right (UpdateBase aMode) ->
                let newBase = base + (readWithMode aMode $ pc + 1)
                 in runHelp $ machine {pc_ = pc + 2, base_ = newBase}
              Left badCode -> pure $ Halted $ BadCode badCode
   in case program of
        PausedAtOutput _ machine -> runHelp machine
        Unstarted machine -> runHelp machine
        Halted _ -> pure program

type Point = (Integer, Integer)

{-| Order matters, since we use `toEnum` -}
data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Enum, Eq)

data Game = Game
  { program_ :: Program
  , tiles_ :: Map Point Tile
  , outputsSoFar_ :: [Integer]
  }

printScreen :: Map Point Tile -> IO ()
printScreen squares =
  let points = M.keys squares
      minX = minimum $ fst <$> points
      minY = minimum $ snd <$> points
      maxX = maximum $ fst <$> points
      maxY = maximum $ snd <$> points
      tileToChar (x, y) =
        case fromMaybe Empty $ M.lookup (x, y) squares of
          Empty -> ' '
          Wall -> 'W'
          Block -> 'B'
          Paddle -> '_'
          Ball -> '*'
      prettyLines =
        fmap
          (\y -> fmap (\x -> tileToChar (x, y)) [minX .. maxX])
          [maxY,maxY - 1 .. minY]
   in putText $ T.unlines $ toS <$> prettyLines

readJoystick :: IO (Maybe Integer)
readJoystick = pure Nothing

runGame :: [Integer] -> IO ()
runGame codes = do
  let step game@(Game program tiles outputsSoFar) = do
        continue <- run readJoystick program
        case (program, outputsSoFar) of
          (Unstarted _, _) -> step $ game {program_ = continue}
          (PausedAtOutput output _, _:[]) ->
            step $
            game {program_ = continue, outputsSoFar_ = outputsSoFar ++ [output]}
          (PausedAtOutput tileCode _, x:y:[]) -> do
            let newTiles =
                  M.insert (x, y) (toEnum $ fromIntegral tileCode) tiles
            printScreen newTiles
            step $
              game {program_ = continue, outputsSoFar_ = [], tiles_ = newTiles}
          (PausedAtOutput output _, _) ->
            step $
            game {program_ = continue, outputsSoFar_ = outputsSoFar ++ [output]}
          (Halted _, _) -> pure ()
  step $ Game {program_ = fromCodes codes, tiles_ = M.empty, outputsSoFar_ = []}

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    traverse readCode <$> T.splitOn "," <$> readFile "Day13-input.txt"
  case parsedCodes of
    Left _ -> putText "Failed to parse codes."
    Right codes -> runGame codes
