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
  GL.PrimitiveMode(..),
  meshPrimitive,

  Material(..),
  identityTexture,
  defaultBaseColorFactor,
  defaultNormalMap,
  defaultMaterial
) where

import Control.Lens
import Codec.Picture
import Data.Map (Map)
import Data.StateVar
import Data.Text (Text)
import Data.Tree
import Data.Tree.Lens
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Foreign
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

type Indices = Vector Word32
type Positions = Vector (V3 Float)
type Normals = Vector (V3 Float)
type Tangents = Vector (V4 Float)
type TexCoords = Vector (V2 Float)

meshPrimitive :: Material
  -> GL.PrimitiveMode
  -> Indices
  -> Positions
  -> Normals
  -> Tangents
  -> TexCoords
  -> IO MeshPrimitive
meshPrimitive material mode ixs positions normals tangents texCoords = do
  -- Create and bind VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Load indices
  let numIxs = length ixs
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  SV.unsafeWith (V.convert ixs) $ \(ptr :: Ptr Word32) -> do
    let size = fromIntegral . (* sizeOf (undefined :: Word32)) $ numIxs
    GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Load vertex data
  vbos <- sequence [
      -- Position
      loadVertexAttribute 0 3 positions,
      -- Normals
      loadVertexAttribute 1 3 normals,
      -- Tangents
      loadVertexAttribute 2 4 tangents,
      -- Texture co-ordinates
      loadVertexAttribute 3 2 texCoords
    ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return . MeshPrimitive material mode vao vbos ebo . fromIntegral $ numIxs
 where
  loadVertexAttribute location components attrs = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    SV.unsafeWith (V.convert attrs) $ \ptr -> do
      let size = fromIntegral . (* sizeOf (undefined :: Float))
                   . (* components) . length $ attrs
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    let attrLoc = GL.AttribLocation location
    GL.vertexAttribPointer attrLoc $=
      (GL.ToFloat,
       GL.VertexArrayDescriptor
         (fromIntegral components)
         GL.Float
         0 -- stride=0, only one attribute per buffer
         nullPtr
      )
    GL.vertexAttribArray attrLoc $= GL.Enabled
    return vbo

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
