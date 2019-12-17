#!/usr/bin/env stack
{- stack script --resolver lts-14.16 --ghc-options -Wall -}
import Protolude (applyN)

type Point = (Int, Int, Int)

newtype Velocity = Velocity
  { getV_ :: Point
  } deriving (Show)

instance Semigroup Velocity where
  (<>) (Velocity (x1, y1, z1)) (Velocity (x2, y2, z2)) =
    Velocity (x1 + x2, y1 + y2, z1 + z2)

instance Monoid Velocity where
  mempty = Velocity (0, 0, 0)

data Moon = Moon
  { position_ :: Point
  , velocity_ :: Velocity
  } deriving (Show)

applyGravity :: Moon -> Moon -> Velocity
applyGravity (Moon (px1, py1, pz1) _) (Moon (px2, py2, pz2) _) =
  let v a b =
        case compare b a of
          GT -> (-1)
          LT -> 1
          EQ -> 0
   in Velocity (v px1 px2, v py1 py2, v pz1 pz2)

applyVelocity :: Moon -> Moon
applyVelocity moon@(Moon (px, py, pz) (Velocity (vx, vy, vz))) =
  moon {position_ = (px + vx, py + vy, pz + vz)}

energy :: Moon -> Int
energy (Moon (px, py, pz) (Velocity (vx, vy, vz))) =
  (abs px + abs py + abs pz) * (abs vx + abs vy + abs vz)

step :: [Moon] -> [Moon]
step moons =
  let afterGravity =
        fmap
          (\a ->
             a
               { velocity_ =
                   (velocity_ a) <> foldMap (\b -> applyGravity b a) moons
               })
          moons
   in applyVelocity <$> afterGravity

main :: IO ()
main = do
  let moons =
        [ Moon (14, 15, -2) mempty
        , Moon (17, -3, 4) mempty
        , Moon (6, 12, -13) mempty
        , Moon (-2, 10, -8) mempty
        ]
  putStr $ "Part 1: " <> (show $ sum $ energy <$> applyN 1000 step moons)
  let stepsToRepeat f count current =
        let nextStep = step current
         in if (f <$> moons) == (f <$> nextStep)
              then count + 1
              else stepsToRepeat f (count + 1) nextStep
  let toX (Moon (px, _, _) (Velocity (vx, _, _))) = (px, vx)
  let toY (Moon (_, py, _) (Velocity (_, vy, _))) = (py, vy)
  let toZ (Moon (_, _, pz) (Velocity (_, _, vz))) = (pz, vz)
  putStr $
    "\nPart 2: " <>
    (show $
     lcm
       (stepsToRepeat toX 0 moons)
       (lcm (stepsToRepeat toY 0 moons) (stepsToRepeat toZ 0 moons)))
