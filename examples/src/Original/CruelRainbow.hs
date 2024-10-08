{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Original.CruelRainbow where

import Original.Spirangle (spirangle)
import Original.Util

output =
  toProgram $
    vec4 (0, 0, 0, 0)
      & const (vec4 (v, v, v, 1))
      & mix 0.1 bb

bb = bbqF (texture2D backBuffer) uvN

bbqF x =
  x
    & lmap (view norm)
    & lmap (rot (w_ audio * 100))
    & lmap (* (x_ audio & copy))
    & rgbF 0.1

v = vqF vq uvN

vqF x = x

-- & lmap (mirrorX)

beaty = beat

vq uv =
  (1 - (sin (beaty)) * x_ (uv * 10) + cos (y_ uv * copy time))
    & (* copy (x_ audio))
