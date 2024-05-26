module App.Render.Model.Model (
  Model(..),

  Node(..),
  _nodeRotation,
  _nodeScale,
  _nodeTranslation,

  G.Skin(..),

  Mesh,

  MeshPrimitive(
    meshPrimMaterial,
    meshPrimGlMode,
    meshPrimVao,
    meshPrimVbo,
    meshPrimEbo,
    meshPrimNumIndices
  ),
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
  nodeMesh :: Mesh,
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
  -- Buffer index buffer
  let numIxs = length ixs
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  SV.unsafeWith (V.convert ixs) $ \(ptr :: Ptr Word32) -> do
    let size = fromIntegral . (* sizeOf (undefined :: Word32)) $ numIxs
    GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Buffer vertex data and set vertex attributes
  let numVertices = V.length positions
  vbos <- sequence [
      -- Vertex attribute location 0: Position
      vertexAttribute 0 positions,
      -- Vertex attribute location 1: Normal
      vertexAttribute 1 normals,
      -- Vertex attribute location 2: Tangents
      vertexAttribute 2 tangents,
      -- Vertex attribute location 3: Texture co-ordinates
      vertexAttribute 3 texCoords,

      -- Optional skinned mesh attributes (We pad joints and weights with
      -- zeros if unused.)

      -- Vertex attribute location 4: Joint
      vertexAttribute 4
        $ joints V.++ V.replicate (numVertices - length joints) 0,
      -- Vertex attribute location 5: Joint weights
      vertexAttribute 5
        $ weights V.++ V.replicate (numVertices - length weights) 0
    ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  return . MeshPrimitive material mode vao vbos ebo . fromIntegral $ numIxs
 where
  vertexAttribute :: forall a. (VertexAttribute a, Storable a)
    => Int
    -> Vector a
    -> IO GL.BufferObject
  vertexAttribute location attrs = do
    let proxy = undefined :: a
    vbo <- bufferVertexData attrs
    bindVertexAttribute proxy vbo location
    return vbo

  bufferVertexData :: forall a. (Storable a) => Vector a -> IO GL.BufferObject
  bufferVertexData attrs = do
    let sv = V.convert attrs
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    SV.unsafeWith sv $ \ptr -> do
      let size = fromIntegral . (* SV.length sv) . sizeOf $ (undefined :: a)
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vbo

  -- Binds vertex attributes for the buffer of type `a` to the currently bound
  -- VAO
  bindVertexAttribute :: (VertexAttribute a)
    => a
    -> GL.BufferObject
    -> Int
    -> IO ()
  bindVertexAttribute proxy vbo locationIndex = do
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    let attrLoc = GL.AttribLocation . fromIntegral $ locationIndex
    GL.vertexAttribPointer attrLoc $=
      (integerHandling proxy,
       GL.VertexArrayDescriptor
         (fromIntegral . attributeComponents $ proxy)
         (glDataType proxy)
         0 -- One attribute per VBO
         nullPtr
      )
    GL.vertexAttribArray attrLoc $= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer $= Nothing

class VertexAttribute a where
  attributeComponents :: a -> Int
  integerHandling :: a -> GL.IntegerHandling
  glDataType :: a -> GL.DataType

instance (VertexAttribute a) => VertexAttribute (V2 a) where
  attributeComponents _ = 2
  integerHandling _ = integerHandling (undefined :: a)
  glDataType _ = glDataType (undefined :: a)

instance (VertexAttribute a) => VertexAttribute (V3 a) where
  attributeComponents _ = 3
  integerHandling _ = integerHandling (undefined:: a)
  glDataType _ = glDataType (undefined:: a)

instance (VertexAttribute a) => VertexAttribute (V4 a) where
  attributeComponents _ = 4
  integerHandling _ = integerHandling (undefined :: a)
  glDataType _ = glDataType (undefined :: a)

instance VertexAttribute Float where
  attributeComponents _ = 1
  integerHandling _ = GL.ToFloat
  glDataType _ = GL.Float

instance VertexAttribute Word16 where
  attributeComponents _ = 1
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
