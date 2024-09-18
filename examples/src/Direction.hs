module Direction where

import Hylogen.WithHylide

forward :: Vec3
forward = vec3 (0, 0, 1)

backward :: Vec3
backward = negate forward

up :: Vec3
up = vec3 (0, 1, 0)

down :: Vec3
down = negate up

right :: Vec3
right = vec3 (1, 0, 0)

left :: Vec3
left = negate right