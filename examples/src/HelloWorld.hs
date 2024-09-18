{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}
{-# HLINT ignore "Redundant bracket" #-}
module HelloWorld where

import Color qualified
import Context qualified
import Direction qualified
import Hylogen.WithHylide
import SDF qualified

output :: Program
output = toProgram scene

sphere :: Vec3 -> Vec1 -> Vec3 -> Vec1
sphere spherePos radius eyePos = len (eyePos - spherePos) - radius

raymarch :: (Fractional iN, Num b2) => (iN -> (Vec3, b2, Booly) -> (Vec3, b2, Booly)) -> Vec4
raymarch f =
  id
    . (\rgb -> vec4 (rgb, 1))
    . (\(rgb, _distance, _continue) -> rgb)
    . foldr
      (f . (/ fromInteger maxSteps) . fromInteger)
      (Color.black, 0, true)
    $ [1 .. maxSteps]
 where
  maxSteps :: Integer
  maxSteps = 64

rot :: Vec1 -> Vec2 -> Vec2
rot phi a =
  vec2
    ( cos phi * x_ a
        + sin phi * y_ a
    , (-1) * sin phi * x_ a
        + cos phi * y_ a
    )

{- | Tile a 3D space
  - c: The size of the tile
  - p: The global position
  - Returns the tile-local position
-}
tesselate :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
tesselate c p = mod_ (p + 0.5 * c) c - 0.5 * c

box :: Vec3 -> Vec3 -> Vec3 -> Vec1
box boxPos dim reference = len (max_ (abs (reference - boxPos) - dim) 0)

scene :: Vec4
scene = raymarch fn
 where
  maxSteps = 64

  -- Camera position
  rayOrigin = vec3 (0, 0, 0)
  rayDirection =
    let
      -- These are total guesses based on observation.
      focalLength = 35
      eye = focalLength / 35 * Direction.backward
      frame = Direction.right ^* uvN.x + Direction.up ^* uvN.y
      preprocessRay p = vec3 (rot (time * 0.4) p.xy, p.z)
     in
      normalize . preprocessRay $ eye + frame

  sdf :: Vec3 -> Vec1
  sdf p =
    let shapeOrigin = vec3 (sin time * 2, cos time * 2, -(3 + sin time * 0.1))
        preprocessPerspective = tesselate 10
     in (box shapeOrigin 0.4) -- Hub
          `min_` (box shapeOrigin (vec3 (0.8, 0.2, 0.2))) -- X arm
          `min_` (box shapeOrigin (vec3 (10, 0.05, 0.05))) -- X spindle
          `min_` (box shapeOrigin (vec3 (0.2, 0.8, 0.2))) -- Y arm
          `min_` (box shapeOrigin (vec3 (0.05, 10, 0.05))) -- Y spindle
          `min_` (box shapeOrigin (vec3 (0.2, 0.2, 0.8))) -- Z arm
          `min_` (box shapeOrigin (vec3 (0.05, 0.05, 10))) -- Z spindle
          $ (preprocessPerspective p)
  fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
  fn iN (color, distCamera, continue) =
    let p = rayOrigin + rayDirection ^* distCamera
        distObject = sdf p

        objColor = (mix iN (vec3 (0.4, 0, 0.2)) (Color.white ^* 0.9))
        newColor = branch (continue * hitObject) objColor color
        newContinue = continue * (not_ hitObject)
        newDistCamera = branch newContinue (distCamera + distObject) distCamera

        hitObject = distObject `lt` 0.001
     in ( newColor
        , newDistCamera
        , newContinue
        )
