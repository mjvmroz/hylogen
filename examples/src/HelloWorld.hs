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

rot2D :: Vec1 -> Vec2 -> Vec2
rot2D phi a =
  vec2
    ( cos phi * x_ a
        + sin phi * y_ a
    , -(sin phi * x_ a)
        + cos phi * y_ a
    )

rotXY :: Vec1 -> Vec3 -> Vec3
rotXY phi p = vec3 (rot2D phi p.xy, p.z)

{- | Tile a rectilinear space in arbitrary dimensions
  - c: The size of the tile
  - p: The global position
  - Returns the tile-local position
-}
tileRectilinear :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
tileRectilinear c p = mod_ (p + 0.5 * c) c - 0.5 * c

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
      focalDistance = focalLength / 35 * Direction.backward
      frame = (vec3 (uvN.xy, 0))
     in
      normalize $ frame + focalDistance
  cameraMovement = rotXY (time * (-0.5)) (vec3 (0, 0, -time))
  sample d =
    rotXY
      (time * 0.4)
      (rayOrigin + cameraMovement + rayDirection ^* d)

  sdf :: Vec3 -> Vec1
  sdf =
    let shapeOrigin = vec3 (sin time * 2, cos time * 2, -(3 + sin time * 0.1))
     in (box shapeOrigin 0.4) -- Hub
          `min_` (box shapeOrigin (vec3 (0.8, 0.2, 0.2))) -- X arm
          `min_` (box shapeOrigin (vec3 (10, 0.05, 0.05))) -- X spindle
          `min_` (box shapeOrigin (vec3 (0.2, 0.8, 0.2))) -- Y arm
          `min_` (box shapeOrigin (vec3 (0.05, 10, 0.05))) -- Y spindle
          `min_` (box shapeOrigin (vec3 (0.2, 0.2, 0.8))) -- Z arm
          `min_` (box shapeOrigin (vec3 (0.05, 0.05, 10))) -- Z spindle
  fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
  fn iN (color, distCamera, continue) =
    let preprocessPerspective = tileRectilinear 10
        distObject = sdf . preprocessPerspective . sample $ distCamera

        objColor = (mix iN (vec3 (0.4, 0, 0.2)) (Color.white ^* 0.9))
        newColor = branch (continue * hitObject) objColor color
        newContinue = continue * (not_ hitObject)
        newDistCamera = branch newContinue (distCamera + distObject) distCamera

        hitObject = distObject `lt` 0.001
     in ( newColor
        , newDistCamera
        , newContinue
        )

-- Framework ideas
-- - Everything is structural by default, but classes can be used to define glsl functions.
-- - Abstract loops. At the moment all loops get unrolled into glsl source, which is not ideal.
-- - Dependency injection can be used to supply them, similarly with uniforms and variables.
-- - Runtime validation can make sure that the names don't collide for multiple inferred glsl types as appropriate
-- - More typelevel representation of semantics, e.g. position, distance, color, direction