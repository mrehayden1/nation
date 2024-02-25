module Render.Model (
  Model,
  SceneNode(..),
  MeshPrimitive(..),
  Material(..),

  traverseModel_,

  fromGlbFile,

  deleteModel
) where

import Control.Monad
import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable as F
import Data.Maybe
import Data.StateVar
import Data.Tree
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V4(..), (!*!))
import qualified Linear as L
import System.IO.Unsafe
import Text.GLTF.Loader (Gltf(..))
import qualified Text.GLTF.Loader as G

import Render.Matrix as M
import qualified Render.Texture as T

type Model = [Tree SceneNode]

type Mesh = Vector MeshPrimitive

-- TODO Increase strictness so all data loads ahead of render.
data SceneNode = SceneNode {
  nodeMesh :: Maybe Mesh,
  nodeMatrix :: L.M44 Float
}

data MeshPrimitive = MeshPrimitive {
  meshPrimMaterial :: Material,
  meshPrimGlMode :: GL.PrimitiveMode,
  meshPrimVao :: GL.VertexArrayObject,
  meshPrimVbo :: [GL.BufferObject],
  meshPrimEbo :: GL.BufferObject,
  meshPrimNumIndices :: GL.NumArrayIndices
}

data Material = Material {
  materialBaseColorFactor :: V4 Float,
  materialBaseColorTexture :: GL.TextureObject,
  materialNormalMap :: GL.TextureObject,
  materialNormalMapScale :: Float,
  materialMetallicRoughnessTexture :: GL.TextureObject
}

traverseModel_ :: Monad m
  => (L.M44 Float -> MeshPrimitive -> m ())
  -> Model
  -> m ()
traverseModel_ render = mapM_ (traverse_ traverseMesh_)
 where
  traverseMesh_ SceneNode{..} = mapM_ (mapM_ (render nodeMatrix)) nodeMesh

fromGlbFile :: FilePath -> IO Model
fromGlbFile pathname = do
  eGlb <- G.fromBinaryFile pathname
  gltf <- case eGlb of
    Left  _ -> fail "loadModel: Failed to read GLB."
    Right g -> return $ G.unGltf g
  let Gltf{..} = gltf
      baseColorTextureIxs = V.catMaybes . fmap getMaterialBaseColorTextureId
        $ gltfMaterials
  textures <- mapM (uncurry $ loadTexture gltfImages gltfSamplers)
                . V.imap (\i t -> (i `elem` baseColorTextureIxs, t))
                $ gltfTextures
  let materials = fmap (loadMaterial textures) gltfMaterials
  meshes <- mapM (mapM (loadMeshPrimitive materials) . G.meshPrimitives) gltfMeshes
  -- Traverse the scene graph and accumulate the transformations
  let scene = makeSceneGraph gltfScenes gltfNodes meshes
  return scene
 where
  getMaterialBaseColorTextureId = pure . G.textureId <=< G.pbrBaseColorTexture <=< G.materialPbrMetallicRoughness

  makeSceneGraph :: Vector G.Scene
    -> Vector G.Node
    -> Vector Mesh
    -> [Tree SceneNode]
  makeSceneGraph scenes nodes meshes =
    let rootIxs = if F.null scenes then mempty else G.sceneNodes $ V.head scenes
    -- Accumulate the transformations recursively and save them in each node
    in F.toList . fmap (accumTransformation L.identity . (nodes !)) $ rootIxs
   where
    accumTransformation :: L.M44 Float -> G.Node -> Tree SceneNode
    accumTransformation m G.Node{..} =
      let s = maybe L.identity M.scale nodeScale
          r = maybe L.identity L.fromQuaternion nodeRotation
          t = fromMaybe 0 nodeTranslation
          tr = L.mkTransformationMat r t
          trs = tr !*! s
          m' = m !*! trs
          ns = F.toList . fmap (accumTransformation m' . (nodes !))
                 $ nodeChildren
          mesh = fmap (meshes !) nodeMeshId
      in Node (SceneNode mesh m') ns

{-# NOINLINE missingImageData #-}
missingImageData :: ByteString
missingImageData = unsafePerformIO $ do
  putStrLn "Reading default image texture."
  BS.readFile "assets/textures/missing-texture.png"

{-# NOINLINE identityTexture #-}
-- Multiplicative identity used when a material has no texture so we have
-- something to multiply the pbrBaseColorFactor by.
identityTexture :: GL.TextureObject
identityTexture =
  let image = (Image 1 1 . SV.fromList $ [255, 255, 255]) :: Image PixelRGB8
  in unsafePerformIO
       . T.fromImage False (T.Nearest, Nothing) T.Nearest T.Repeat T.Repeat
       $ image

defaultBaseColorFactor :: Num a => L.V4 a
defaultBaseColorFactor = 1

defaultNormalMap :: GL.TextureObject
defaultNormalMap =
  let image = (Image 1 1 . SV.fromList $ [0, 0, 255]) :: Image PixelRGB8
  in unsafePerformIO
       . T.fromImage False (T.Nearest, Nothing) T.Nearest T.Repeat T.Repeat
       $ image

defaultMaterial :: Material
defaultMaterial = Material {
    materialBaseColorFactor = defaultBaseColorFactor,
    materialBaseColorTexture = identityTexture,
    materialNormalMap = defaultNormalMap,
    materialNormalMapScale = 1,
    materialMetallicRoughnessTexture = identityTexture
  }

loadTexture :: Vector G.Image
  -> Vector G.Sampler
  -> T.SRGB
  -> G.Texture
  -> IO GL.TextureObject
loadTexture images samplers srgb G.Texture{..} = do
  let mSampler = fmap (samplers !) textureSamplerId
      wrapS = maybe T.Repeat (toWrapMode . G.samplerWrapS) mSampler
      wrapT = maybe T.Repeat (toWrapMode . G.samplerWrapS) mSampler
      minF = maybe (T.Nearest, Nothing) toMinFilter
               $ G.samplerMinFilter =<< mSampler
      magF = maybe T.Nearest toMagFilter
               $ G.samplerMagFilter =<< mSampler
  let image = fromMaybe missingImageData
                $ G.imageData . (images !) =<< textureSourceId
  T.decodeImage srgb minF magF wrapS wrapT image
 where
  toMinFilter :: G.MinFilter -> T.MinificationFilter
  toMinFilter f = case f of
    G.MinNearest              -> (T.Nearest, Nothing)
    G.MinLinear               -> (T.Linear', Nothing)
    G.MinNearestMipmapNearest -> (T.Nearest, Just T.Nearest)
    G.MinLinearMipmapNearest  -> (T.Linear', Just T.Nearest)
    G.MinNearestMipmapLinear  -> (T.Nearest, Just T.Linear')
    G.MinLinearMipmapLinear   -> (T.Linear', Just T.Linear')

  toMagFilter :: G.MagFilter -> T.MagnificationFilter
  toMagFilter f = case f of
    G.MagLinear  -> T.Linear'
    G.MagNearest -> T.Nearest

  toWrapMode :: G.SamplerWrap -> T.TextureWrapMode
  toWrapMode w = case w of
    G.ClampToEdge    -> T.ClampToEdge
    G.MirroredRepeat -> T.MirroredRepeat
    G.Repeat         -> T.Repeat

loadMaterial :: Vector GL.TextureObject -> G.Material -> Material
loadMaterial textures G.Material{..} =
  -- TODO Apply vertex color and baseColorFactor weights to baseColorTexture
  let colorFactor = maybe defaultBaseColorFactor G.pbrBaseColorFactor
                      materialPbrMetallicRoughness
      colorTexture =
        maybe identityTexture ((textures !) . G.textureId)
          $ G.pbrBaseColorTexture =<< materialPbrMetallicRoughness
  -- TODO Apply scaling factor to normal map
      normalMap = maybe defaultNormalMap ((textures !) . G.normalTextureId)
                    materialNormalTexture
      normalMapScale = maybe 1 G.normalTextureScale materialNormalTexture
  -- TODO Apply metallic roughness factors to metallicRoughnessTexture
      metallicRoughness =
        maybe identityTexture ((textures !) . G.textureId)
          $ G.pbrMetallicRoughnessTexture =<< materialPbrMetallicRoughness
  in Material colorFactor colorTexture normalMap normalMapScale metallicRoughness

loadMeshPrimitive :: Vector Material -> G.MeshPrimitive -> IO MeshPrimitive
loadMeshPrimitive materials G.MeshPrimitive{..} = do
  -- Create and bind VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Load indices
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  SV.unsafeWith (V.convert meshPrimitiveIndices) $ \(ptr :: Ptr Word32) -> do
    let size = fromIntegral . (* sizeOf (undefined :: Word32)) . length
                 $ meshPrimitiveIndices
    GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Load vertex data
  vbos <- sequence [
      -- Position
      loadVertexAttribute 0 3 meshPrimitivePositions,
      -- Normals
      loadVertexAttribute 1 3 meshPrimitiveNormals,
      -- Tangents
      loadVertexAttribute 2 4 meshPrimitiveTangents,
      -- Texture co-ordinates
      loadVertexAttribute 3 2 meshPrimitiveTexCoords
    ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  let mode = toGlPrimitiveMode meshPrimitiveMode
      material = maybe defaultMaterial (materials !) meshPrimitiveMaterial
  return . MeshPrimitive material mode vao vbos ebo . fromIntegral . length
    $ meshPrimitiveIndices
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

toGlPrimitiveMode :: G.MeshPrimitiveMode -> GL.PrimitiveMode
toGlPrimitiveMode p = case p of
  G.Lines         -> GL.Lines
  G.LineLoop      -> GL.LineLoop
  G.LineStrip     -> GL.LineStrip
  G.Points        -> GL.Points
  G.Triangles     -> GL.Triangles
  G.TriangleFan   -> GL.TriangleFan
  G.TriangleStrip -> GL.TriangleStrip

deleteModel :: Model -> IO ()
deleteModel = undefined
