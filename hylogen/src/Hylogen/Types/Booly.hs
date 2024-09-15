{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Hylogen.Types.Booly where

import Data.Bool (otherwise)
import Data.Boolean
import Data.Function (id)
import Data.Ord
import GHC.Num
import Hylogen.Expr

-- | Booly singleton type tag
data BoolyType = BoolyType

instance ToGLSLType BoolyType where
  toGLSLType _ = GLSLBool
  tag = BoolyType

-- | Hylogen Boolean type
type Booly = Expr BoolyType

-- | We use Num operators for Boolean arithmetic:
instance Num Booly where
  (+) = op2 "||"
  (*) = op2 "&&"
  negate = op1 "!"
  abs = id
  signum = id
  fromInteger x
    | x > 0 = uniform "true"
    | otherwise = uniform "false"

instance Boolean Booly where
  true = uniform "true"
  false = uniform "false"
  notB = negate
  (&&*) = (*)
  (||*) = (+)