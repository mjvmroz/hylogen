{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Hylogen.Types.Texture where

import Hylogen.Expr

-- | Texture singleton type tag
data TextureType = TextureType

instance ToGLSLType TextureType where
  toGLSLType _ = GLSLTexture
  tag = TextureType

-- | Hylogen Texture type
type Texture = Expr TextureType
