module Context where

import Hylogen.WithHylide

-- | The backbuffer, with a transformation applied to the coordinate selection
bbSkew :: (Vec2 -> Vec2) -> Vec4
bbSkew transformCoords = texture2D backBuffer (0.5 * transformCoords uvN + 0.5)

-- | Just the back buffer, but in a convenient vec4 representation
bb :: Vec4
bb = bbSkew id