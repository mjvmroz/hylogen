{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Original.Starter where

import Original.Util

output :: Program
output = toProgram $ vec4 (v, v, v, 1)

v = vqF vq uvN

vqF x = x

vq uv = 0.5
