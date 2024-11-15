{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}
{-# HLINT ignore "Redundant bracket" #-}
module HelloWorld where

import Color qualified
import Context qualified
import Data.Boolean
import Direction qualified
import Hylogen.WithHylide
import SDF qualified

output :: Program
output = toProgram scene

sphere :: Vec3 -> Vec1 -> Vec3 -> Vec1
sphere spherePos radius eyePos = len (eyePos - spherePos) - radius

raymarch :: (Fractional iN, Num dist, Boolean continue) => (iN -> (Vec3, dist, continue) -> (Vec3, dist, continue)) -> Vec4
raymarch takeStep =
  id
    . (\rgb -> vec4 (rgb, 1))
    . (\(rgb, _distance, _continue) -> rgb)
    . foldr
      (takeStep . (/ fromInteger maxSteps) . fromInteger)
      (Color.black, 0, true)
    $ [1 .. maxSteps]
  where
    maxSteps :: Integer
    maxSteps = 64

rot2D :: Vec1 -> Vec2 -> Vec2
rot2D phi a =
  vec2
    ( cos phi * a.x
        + (sin phi * a.y),
      cos phi * a.y
        - (sin phi * a.x)
    )

rotXY :: Vec1 -> Vec3 -> Vec3
rotXY phi p = vec3 (rot2D phi p.xy, p.z)

-- | Tile a rectilinear space in arbitrary dimensions
tileRectilinear ::
  forall n.
  (Veccable n) =>
  -- | c: The size of the tile
  Vec n ->
  -- | p: The global position
  Vec n ->
  -- | Returns the tile-local position
  Vec n
tileRectilinear c p = mod_ (p + 0.5 * c) c - 0.5 * c

cube :: Vec3 -> Vec3 -> Vec3 -> Vec1
cube boxPos dim reference = len (max_ (abs (reference - boxPos) - dim) 0)

scene :: Vec4
scene = raymarch fn + 0.1 * blurryReflection
  where
    blurryReflection = Context.bbSkew (\p -> vec2 (p.x + (sin time), p.y + (cos time)))

    -- Camera position
    rayOrigin = vec3 (0, 0, 0)
    rayDirection =
      let -- These are total guesses based on observation.
          focalLength = 35
          focalDistance = focalLength / 35 * Direction.backward
          frame = (vec3 (uvN.xy, 0))
       in normalize $ frame + focalDistance
    cameraMovement = rotXY (time * (-0.5)) (vec3 (0, 0, -time))
    rotateCamera = rotXY $ time * 0.4
    sample distance = rotateCamera $ rayOrigin + cameraMovement + rayDirection ^* distance

    sdf :: Vec3 -> Vec1
    sdf =
      let shapeOrigin = vec3 (sin time * 2, cos time * 2, -(3 + sin time * 0.1))
       in minOf_
            [ (sphere shapeOrigin 0.7), -- Hub
              (cube shapeOrigin (vec3 (0.8, 0.2, 0.2))), -- X arm
              (cube shapeOrigin (vec3 (10, 0.05, 0.05))), -- X spindle
              (cube shapeOrigin (vec3 (0.2, 0.8, 0.2))), -- Y arm
              (cube shapeOrigin (vec3 (0.05, 10, 0.05))), -- Y spindle
              (cube shapeOrigin (vec3 (0.2, 0.2, 0.8))), -- Z arm
              (cube shapeOrigin (vec3 (0.05, 0.05, 10))) -- Z spindle
            ]
    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn iN (color, distCamera, continue) =
      let preprocessPerspective = tileRectilinear 10
          distObject = sdf . preprocessPerspective . sample $ distCamera

          objColor = (mix iN (vec3 (0.4, 0, 0.2)) (Color.white ^* 0.9))
          newColor = branch (continue &&* hitObject) objColor color
          newContinue = continue &&* (not_ hitObject)
          -- Floating points suck, and we can improve things by going only part of the
          -- way to the object. This introduces a kind of "darkness creep": a fog which
          -- is not present in the scene, but which effectively causes light to decay
          -- as it travels through it. The "darkness cost" of decreasing the undershoot
          -- factor can be balanced by increasing the number of steps in the raymarch.
          undershootDist = distObject * 0.98
          newDistCamera = branch newContinue (distCamera + undershootDist) distCamera

          hitObject = distObject `lt` 0.001
       in ( newColor,
            newDistCamera,
            newContinue
          )

-- Framework ideas
-- - Everything is structural by default, but classes can be used to define glsl functions.
-- - Abstract loops. At the moment all loops get unrolled into glsl source, which is not ideal.
-- - Dependency injection can be used to supply them, similarly with uniforms and variables.
-- - Runtime validation can make sure that the names don't collide for multiple inferred glsl types as appropriate
-- - More typelevel representation of semantics, e.g. position, distance, color, direction
