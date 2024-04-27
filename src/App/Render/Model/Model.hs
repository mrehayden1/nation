module App.Render.Model.Model (
  Model(..),

  Node(..),
  _nodeRotation,
  _nodeScale,
  _nodeTranslation,

  G.Skin(..),

  Mesh,

  MeshPrimitive(..),
  GL.PrimitiveMode(..),
  meshPrimitive,

  Material(..),
  defaultBaseColorFactor,
  defaultMaterial,
  defaultMetallicFactor,
  defaultRoughnessFactor
) where

import Control.Lens
import Data.Map (Map)
import Data.Proxy
import Data.StateVar
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear
import qualified Text.GLTF.Loader as G

data Model = Model {
  modelNodes :: Vector Node,
  modelScene :: Vector Int, -- node indices
  modelSkins :: Vector G.Skin,
  modelTranslation :: V3 Float
} deriving (Show)

-- TODO Do we need to increase strictness so all data loads ahead of render?
data Node = Node {
  nodeAnimations :: Map Text [G.Channel],
  nodeChildren :: Vector Int,
  nodeIsJoint :: Bool,
  nodeMesh :: Maybe Mesh,
  nodeRotation :: Quaternion Float,
  nodeScale :: V3 Float,
  nodeSkin :: Maybe Int,
  nodeTranslation :: V3 Float
} deriving (Show)

_nodeRotation :: Lens' Node (Quaternion Float)
_nodeRotation = lens nodeRotation (\n r -> n { nodeRotation = r })

_nodeScale :: Lens' Node (V3 Float)
_nodeScale = lens nodeScale (\n s -> n { nodeScale = s })

_nodeTranslation :: Lens' Node (V3 Float)
_nodeTranslation = lens nodeTranslation (\n t -> n { nodeTranslation = t })

type Mesh = Vector MeshPrimitive

data MeshPrimitive = MeshPrimitive {
  meshPrimMaterial :: Material,
  meshPrimGlMode :: GL.PrimitiveMode,
  meshPrimVao :: GL.VertexArrayObject,
  meshPrimVbo :: [GL.BufferObject],
  meshPrimEbo :: GL.BufferObject,
  meshPrimNumIndices :: GL.NumArrayIndices
} deriving (Show)

type Indices = Vector Word32
type Positions = Vector (V3 Float)
type Normals = Vector (V3 Float)
type Tangents = Vector (V4 Float)
type TexCoords = Vector (V2 Float)
type Joints = Vector (V4 Word16)
type Weights = Vector (V4 Float)

meshPrimitive :: Material
  -> GL.PrimitiveMode
  -> Indices
  -> Positions
  -> Normals
  -> Tangents
  -> TexCoords
  -> Joints
  -> Weights
  -> IO MeshPrimitive
meshPrimitive material mode ixs positions normals tangents texCoords joints weights = do
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
  let numVertices = V.length positions
  vbos <- sequence [
      loadVertexAttribute 0 3 positions,
      loadVertexAttribute 1 3 normals,
      loadVertexAttribute 2 4 tangents,
      loadVertexAttribute 3 2 texCoords,
      -- Pad joints and weights with zeros if empty.
      loadVertexAttribute 4 4
        $ joints V.++ V.replicate (numVertices - length joints) 0,
      loadVertexAttribute 5 4
        $ weights V.++ V.replicate (numVertices - length weights) 0
    ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return . MeshPrimitive material mode vao vbos ebo . fromIntegral $ numIxs
 where
  loadVertexAttribute :: forall a. (Storable a, VertexAttribute a)
    => Int
    -> Int
    -> Vector a
    -> IO GL.BufferObject
  loadVertexAttribute location components attrs = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    SV.unsafeWith (V.convert attrs) $ \ptr -> do
      let size = fromIntegral . (* sizeOf (undefined :: a))
                   . (* components) . length $ attrs
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    let attrLoc = GL.AttribLocation . fromIntegral $ location
    GL.vertexAttribPointer attrLoc $=
      (integerHandling (Proxy :: Proxy a),
       GL.VertexArrayDescriptor
         (fromIntegral components)
         (glDataType (Proxy :: Proxy a))
         0 -- stride=0, only one attribute per buffer
         nullPtr
      )
    GL.vertexAttribArray attrLoc $= GL.Enabled
    return vbo

class VertexAttribute a where
  integerHandling :: Proxy a -> GL.IntegerHandling
  glDataType :: Proxy a -> GL.DataType

instance VertexAttribute a => VertexAttribute (V2 a) where
  integerHandling _ = integerHandling (Proxy :: Proxy a)
  glDataType _ = glDataType (Proxy :: Proxy a)

instance VertexAttribute a => VertexAttribute (V3 a) where
  integerHandling _ = integerHandling (Proxy :: Proxy a)
  glDataType _ = glDataType (Proxy :: Proxy a)

instance VertexAttribute a => VertexAttribute (V4 a) where
  integerHandling _ = integerHandling (Proxy :: Proxy a)
  glDataType _ = glDataType (Proxy :: Proxy a)

instance VertexAttribute Float where
  integerHandling _ = GL.ToFloat
  glDataType _ = GL.Float

instance VertexAttribute Word16 where
  integerHandling _ = GL.KeepIntegral
  glDataType _ = GL.UnsignedShort

data Material = Material {
  materialAlphaMode :: G.MaterialAlphaMode,
  materialAlphaCutoff :: Float,
  materialBaseColorFactor :: V4 Float,
  materialBaseColorTexture :: Maybe GL.TextureObject,
  materialDoubleSided :: Bool,
--materialEmissiveFactor :: Float,
--materialEmissiveTexture :: Maybe GL.TextureObject,
  materialMetallicFactor :: Float,
  materialMetallicRoughnessTexture :: Maybe GL.TextureObject,
  materialNormalTexture :: Maybe GL.TextureObject,
  materialNormalTextureScale :: Float,
--materialOcclusionTexture :: Maybe GL.TextureObject,
--materialOcclusionTextureStrength :: Float,
  materialRoughnessFactor :: Float
} deriving (Show)

defaultBaseColorFactor :: Num a => V4 a
defaultBaseColorFactor = 1

defaultMetallicFactor :: Float
defaultMetallicFactor = 1

defaultRoughnessFactor :: Float
defaultRoughnessFactor = 1

defaultMaterial :: Material
defaultMaterial = Material {
    materialAlphaCutoff = 0.5,
    materialAlphaMode = G.Opaque,
    materialBaseColorFactor = defaultBaseColorFactor,
    materialBaseColorTexture = Nothing,
    materialDoubleSided = False,
--  materialEmissiveFactor = 0,
--  materialEmissiveTexture = Nothing,
    materialMetallicFactor = defaultMetallicFactor,
    materialMetallicRoughnessTexture = Nothing,
    materialNormalTexture = Nothing,
    materialNormalTextureScale = 1,
--  materialOcclusionTexture = Nothing,
--  materialOcclusionTextureStrength = 1,
    materialRoughnessFactor = 1
  }
