{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Expr
import Hylogen.Types

-- | Length of a vector
len :: forall n. (Veccable n) => Vec n -> Vec1
len = op1pre "length"

-- | Euclidean distance between two points
distance :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
distance = op2pre'' "distance"

cross :: Vec3 -> Vec3 -> Vec3
cross = op2pre'' "cross"

normalize :: forall n. (Veccable n) => Vec n -> Vec n
normalize = op1pre'' "normalize"

smoothstep :: Vec1 -> Vec1 -> Vec1 -> Vec1
smoothstep = op3pre'' "smoothstep"

-- | Returns a vector pointing in the same direction as another
--
-- @
-- faceforward toOrient incident reference  -- == oriented
-- @
faceForward :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
faceForward = op3pre'' "faceforward"

-- | Calculates the reflection direction for an incident vector
--
-- @
-- reflect incident normal -- == reflected
-- @
reflect :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
reflect = op2pre'' "reflect"

-- | Calculates the refraction direction direction for an incident vector
--
-- @
-- refract incident normal eta -- == reflected
-- @
--
-- where eta is the ratio of indicies of refraction
refract :: forall n. (Veccable n) => Vec n -> Vec n -> Vec1 -> Vec n
refract = op3pre "refract"

inverseSqrt :: forall n. (Veccable n) => Vec n -> Vec n
inverseSqrt = op1pre'' "inversesqrt"

-- | Fractional part
fract :: forall n. (Veccable n) => Vec n -> Vec n
fract = op1pre'' "fract"

mod_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
mod_ = op2pre'' "mod"

floor_ :: forall n. (Veccable n) => Vec n -> Vec n
floor_ = op1pre'' "floor"

ceil_ :: forall n. (Veccable n) => Vec n -> Vec n
ceil_ = op1pre'' "ceil"

class Comparable a where
  min_ :: a -> a -> a
  max_ :: a -> a -> a

-- | Returns the minimum of n values
--
--   WARNING: This function is non-total (@minOf [] = undefined@)
minOf_ :: (Comparable a) => [a] -> a
minOf_ = foldl1 min_

-- | Returns the maximum of n values
--
--   WARNING: This function is non-total (@maxOf [] = undefined@)
maxOf_ :: (Comparable a) => [a] -> a
maxOf_ = foldl1 max_

instance (Comparable b) => Comparable (a -> b) where
  min_ f g x = min_ (f x) (g x)
  max_ f g x = max_ (f x) (g x)

instance (Veccable n) => Comparable (Vec n) where
  min_ = op2pre'' "min"

  max_ = op2pre'' "max"

-- | Clamps x between min and max
--
-- @
-- clamp min max x -- == clamped
-- @
clamp :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
clamp mn mx x = op3pre'' "clamp" x mn mx

-- | Linear interpolation between x and y by p, a Vec1 from 0 to 1
--
-- @
-- mix p x y = x ^* (1 - p) + y ^* p
-- -- mix 0 x y == x
-- -- mix 1 x y == y
-- @
mix :: (Veccable n) => Vec1 -> Vec n -> Vec n -> Vec n
mix p x y = op3pre "mix" x y p

-- | Helper function to compare vectors
bcomp :: (Veccable v) => String -> Vec v -> Vec v -> Booly
bcomp str x y = product $ zipWith (op2' str) (toList x) (toList y)

eq :: (Veccable v) => Vec v -> Vec v -> Booly
eq = bcomp "=="

neq :: (Veccable v) => Vec v -> Vec v -> Booly
neq = bcomp "!="

lt :: (Veccable v) => Vec v -> Vec v -> Booly
lt = bcomp "<"

gt :: (Veccable v) => Vec v -> Vec v -> Booly
gt = bcomp ">"

leq :: (Veccable v) => Vec v -> Vec v -> Booly
leq = bcomp "<="

geq :: (Veccable v) => Vec v -> Vec v -> Booly
geq = bcomp ">="

-- | Returns rgba value given a texture and texture coordinates
-- texture coordinates start at 0 1
texture2D :: Texture -> Vec2 -> Vec4
texture2D = op2pre "texture2D"

-- | Selection function
--
-- @ sel bool x y @
-- is akin to
--
-- @ bool ? x : y @ in C-like languages
sel ::
  forall a.
  (ToGLSLType a) =>
  Booly ->
  Expr a ->
  Expr a ->
  Expr a
sel a b c = Expr t (Tree (Select, toGLSLType t, "") [toMono a, toMono b, toMono c])
  where
    t = tag :: a

branch :: (ToGLSLType a) => Booly -> Expr a -> Expr a -> Expr a
branch = sel

------------------------------------------------------------

-- | Conditional expressions
--  Syntax sugar for `sel`:
--  `cond ? ifTrue :? ifFalse`
(?) :: (ToGLSLType a, e ~ Expr a) => Booly -> (e, e) -> e
cond ? (ifTrue, ifFalse) = sel cond ifTrue ifFalse

and_ :: Booly -> Booly -> Booly
and_ = op2' "&&"

or_ :: Booly -> Booly -> Booly
or_ = op2' "||"

not_ :: Booly -> Booly
not_ = op1 "!"
