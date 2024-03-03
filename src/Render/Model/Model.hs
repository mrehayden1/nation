module Render.Model.Model (
  Model(..),
  modelScene,
  modelTranslation,

  SceneNode(..),
  nodeAnimations,
  nodeMesh,
  nodeRotation,
  nodeScale,
  nodeTranslation,

  Mesh,
  MeshPrimitive(..),

  Material(..),
  identityTexture,
  defaultBaseColorFactor,
  defaultNormalMap,
  defaultMaterial
) where

import Control.Lens
import Codec.Picture
import Data.Map (Map)
import Data.Text (Text)
import Data.Tree
import Data.Tree.Lens
import Data.Vector (Vector)
import qualified Data.Vector.Storable as SV
import qualified Graphics.Rendering.OpenGL as GL
import Linear
import System.IO.Unsafe
import qualified Text.GLTF.Loader as G

import qualified Render.Texture as T

data Model = Model {
  _modelScene :: Tree SceneNode
}

-- TODO Do we need to increase strictness so all data loads ahead of render?
data SceneNode = SceneNode {
  _nodeAnimations :: Map Text [G.Channel],
  _nodeMesh :: Maybe Mesh,
  -- FIXME Handle optional transformation matrices (can be used instead of TRS)
  _nodeRotation :: Quaternion Float,
  _nodeScale :: V3 Float,
  _nodeTranslation :: V3 Float
}

type Mesh = Vector MeshPrimitive

data MeshPrimitive = MeshPrimitive {
  meshPrimMaterial :: Material,
  meshPrimGlMode :: GL.PrimitiveMode,
  meshPrimVao :: GL.VertexArrayObject,
  meshPrimVbo :: [GL.BufferObject],
  meshPrimEbo :: GL.BufferObject,
  meshPrimNumIndices :: GL.NumArrayIndices
}

data Material = Material {
  materialAlphaMode :: G.MaterialAlphaMode,
  materialAlphaCutoff :: Float,
  materialBaseColorFactor :: V4 Float,
  materialBaseColorTexture :: GL.TextureObject,
  materialDoubleSided :: Bool,
  materialNormalMap :: GL.TextureObject,
  materialNormalMapScale :: Float,
  materialMetallicRoughnessTexture :: GL.TextureObject
}

{-# NOINLINE identityTexture #-}
-- Multiplicative identity used when a material has no texture so we have
-- something to multiply the pbrBaseColorFactor by.
identityTexture :: GL.TextureObject
identityTexture =
  let image = (Image 1 1 . SV.fromList $ [255, 255, 255]) :: Image PixelRGB8
  in unsafePerformIO
       . T.fromImage False (T.Nearest, Nothing) T.Nearest T.Repeat T.Repeat
       $ image

defaultBaseColorFactor :: Num a => V4 a
defaultBaseColorFactor = 1

defaultNormalMap :: GL.TextureObject
defaultNormalMap =
  let image = (Image 1 1 . SV.fromList $ [0, 0, 255]) :: Image PixelRGB8
  in unsafePerformIO
       . T.fromImage False (T.Nearest, Nothing) T.Nearest T.Repeat T.Repeat
       $ image

defaultMaterial :: Material
defaultMaterial = Material {
    materialAlphaCutoff = 0.5,
    materialAlphaMode = G.Opaque,
    materialBaseColorFactor = defaultBaseColorFactor,
    materialBaseColorTexture = identityTexture,
    materialDoubleSided = False,
    materialNormalMap = defaultNormalMap,
    materialNormalMapScale = 1,
    materialMetallicRoughnessTexture = identityTexture
  }

$(makeLenses ''Model)
$(makeLenses ''SceneNode)

modelTranslation :: Lens' Model (V3 Float)
modelTranslation = modelScene . root . nodeTranslation
