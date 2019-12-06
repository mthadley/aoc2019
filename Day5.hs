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
        if i `div` (10 ^ (p - 1)) `mod` 10 > 0
          then Immediate
          else Position
   in case i `mod` 100 of
        1 -> Right $ Add (mode 3) (mode 4)
        2 -> Right $ Multiply (mode 3) (mode 4)
        3 -> Right $ Save
        4 -> Right $ Output (mode 3)
        5 -> Right $ JumpIfTrue (mode 3) (mode 4)
        6 -> Right $ JumpIfFalse (mode 3) (mode 4)
        99 -> Right Halt
        badCode -> Left badCode

run :: Int -> S.Seq Int -> IO (S.Seq Int)
run input =
  let runHelp pc codes =
        let readWithMode mode pos =
              let val = S.index codes pos
               in case mode of
                    Position -> S.index codes val
                    Immediate -> val
            runWith f aMode bMode =
              let inputA = readWithMode aMode $ pc + 1
                  inputB = readWithMode bMode $ pc + 2
                  output = S.index codes $ pc + 3
               in runHelp (pc + 4) (S.update output (f inputA inputB) codes)
            jumpWith f aMode jMode =
              let val = readWithMode aMode (pc + 1)
                  nextPC =
                    if f val
                      then readWithMode jMode (pc + 1)
                      else pc + 3
               in runHelp nextPC codes
            compareWith f aMode bMode =
              let val =
                    if (readWithMode aMode (pc + 1) `f`
                        readWithMode bMode (pc + 2))
                      then 1
                      else 0
                  addr = S.index codes $ pc + 3
               in runHelp (pc + 4) $ S.update addr val codes
         in case decodeInstruction $ S.index codes pc of
              Right (Add aMode bMode) -> runWith (+) aMode bMode
              Right (Multiply aMode bMode) -> runWith (*) aMode bMode
              Right Halt -> pure codes
              Right Save ->
                let addr = S.index codes $ pc + 1
                 in runHelp (pc + 2) $ S.update addr input codes
              Right (Output oMode) -> do
                print $ readWithMode oMode (pc + 1)
                runHelp (pc + 2) codes
              Right (JumpIfTrue aMode jMode) -> jumpWith ((/=) 0) aMode jMode
              Right (JumpIfFalse aMode jMode) -> jumpWith ((==) 0) aMode jMode
              Right (LessThan aMode bMode) -> compareWith (<) aMode bMode
              Right (Equals aMode bMode) -> compareWith (==) aMode bMode
              Left badCode -> fail $ "Hit a bad code: " <> (show badCode)
   in runHelp 0

main :: IO ()
main = do
  let readCode t = fst <$> signed decimal t
  parsedCodes <-
    fmap S.fromList <$> traverse readCode <$> T.splitOn "," <$>
    readFile "Day5-input.txt"
  case parsedCodes of
    Left _ -> putLText "Failed to parse codes."
    Right codes -> do
      putText "Part 1: \n"
      _ <- run 1 codes
      putText "\nPart 2: \n"
      _ <- run 5 codes
      pure ()
