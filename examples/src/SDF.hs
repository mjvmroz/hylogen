{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SDF where

import Data.Set.NonEmpty qualified as NES
import GHC.Float
import GHC.Num
import GHC.Real
import Hylogen.WithHylide

--------------------------------------------------------------------------------

class SDF a where
  runSDF :: Context -> a -> Vec1

data Context = Context
  { eye :: Vec3
  }

data Primitive = Primitive {unPrimitive :: Context -> Vec1}

sphere :: Vec3 -> Vec1 -> Primitive
sphere center radius = Primitive $ \ctx -> len (ctx.eye - center) - radius

data Union = Union {unUnion :: NES.NESet Primitive}

data Intersection = Intersection {unIntersection :: NES.NESet Primitive}

data Average = Average {unAverage :: NES.NESet Primitive}

instance Num Primitive where
  fromInteger i = Primitive $ const $ fromInteger i
  Primitive a + Primitive b = Primitive $ \p -> min_ (a p) (b p)
  Primitive a * Primitive b = Primitive $ \p -> a p + b p
  Primitive a - Primitive b = Primitive $ \p -> max_ (a p) (negate $ b p)
  negate (Primitive a) = Primitive $ \p -> negate $ a p
  abs (Primitive a) = Primitive $ \p -> abs $ a p
  signum (Primitive a) = Primitive $ \p -> signum $ a p

instance Fractional Primitive where
  fromRational r = Primitive $ const $ fromRational r
  recip (Primitive a) = Primitive $ \p -> recip $ a p

instance Floating Primitive where
  pi = Primitive $ const pi
  exp (Primitive a) = Primitive $ \p -> exp $ a p
  log (Primitive a) = Primitive $ \p -> log $ a p
  sqrt (Primitive a) = Primitive $ \p -> sqrt $ a p
  (**) (Primitive a) (Primitive b) = Primitive $ \p -> a p ** b p
  logBase (Primitive a) (Primitive b) = Primitive $ \p -> logBase (a p) (b p)
  sin (Primitive a) = Primitive $ \p -> sin $ a p
  cos (Primitive a) = Primitive $ \p -> cos $ a p
  tan (Primitive a) = Primitive $ \p -> tan $ a p
  asin (Primitive a) = Primitive $ \p -> asin $ a p
  acos (Primitive a) = Primitive $ \p -> acos $ a p
  atan (Primitive a) = Primitive $ \p -> atan $ a p
  sinh (Primitive a) = Primitive $ \p -> sinh $ a p
  cosh (Primitive a) = Primitive $ \p -> cosh $ a p
  tanh (Primitive a) = Primitive $ \p -> tanh $ a p
  asinh (Primitive a) = Primitive $ \p -> asinh $ a p
  acosh (Primitive a) = Primitive $ \p -> acosh $ a p
  atanh (Primitive a) = Primitive $ \p -> atanh $ a p
