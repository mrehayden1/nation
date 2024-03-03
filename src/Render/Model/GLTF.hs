module Render.Model.GLTF (
  fromGlbFile
) where

import Control.Monad
import Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.StateVar
import Data.Text (Text)
import Data.Tree
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear
import Text.GLTF.Loader (Gltf(..))
import qualified Text.GLTF.Loader as G
import Text.Printf

import qualified Render.Model.GLTF.Material as M
import Render.Model.GLTF.Texture as GT
import Render.Model.Model

fromGlbFile :: FilePath -> IO Model
fromGlbFile pathname = do
  printf "Loading model \"%s\"...\n" pathname
  eGlb <- G.fromBinaryFile pathname
  gltf <- case eGlb of
    Left  _ -> fail "fromGlbFile: Failed to read GLB."
    Right g -> return $ G.unGltf g
  let Gltf{..} = gltf
      baseColorTextureIxs = V.catMaybes . fmap getMaterialBaseColorTextureId
        $ gltfMaterials
  textures <- mapM (uncurry $ loadTexture gltfImages gltfSamplers)
                . V.imap (\i t -> (i `elem` baseColorTextureIxs, t))
                $ gltfTextures
  let materials = fmap (M.adaptMaterial textures) gltfMaterials
  meshes <- mapM (mapM (loadMeshPrimitive materials) . G.meshPrimitives) gltfMeshes
  let scene = makeSceneGraph gltfScenes gltfNodes gltfAnimations meshes
  return $ Model scene
 where
  getMaterialBaseColorTextureId = pure . G.textureId <=< G.pbrBaseColorTexture <=< G.materialPbrMetallicRoughness

  makeSceneGraph :: Vector G.Scene
    -> Vector G.Node
    -> Vector G.Animation
    -> Vector Mesh
    -> Tree SceneNode
  makeSceneGraph scenes nodes animations meshes =
    let rootIxs = G.sceneNodes . V.head $ scenes
        -- Index every animation channel by node ID
        animMap = indexAnimations . toList $ animations
    in Node (SceneNode mempty Nothing (Quaternion 1 0) 1 0) . F.toList
         . fmap (makeNode animMap) $ rootIxs
   where
    makeNode :: Map Text (Map Int [G.Channel]) -> Int -> Tree SceneNode
    makeNode animMap i =
      let n = nodes ! i
          r = fromMaybe (Quaternion 1 0) . G.nodeRotation $ n
          s = fromMaybe 1 . G.nodeScale $ n
          t = fromMaybe 0 . G.nodeTranslation $ n
          ns = F.toList . fmap (makeNode animMap) . G.nodeChildren $ n
          m = fmap (meshes !) . G.nodeMeshId $ n
          as = M.mapMaybe (M.lookup i) animMap
      in Node (SceneNode as m r s t) ns

  indexAnimations :: [G.Animation] -> Map Text (Map Int [G.Channel])
  indexAnimations = M.fromList . fmap ((,) <$> animationName <*> indexChannels)
   where
    animationName = fromMaybe (error "Animation name required")
                      . G.animationName

    indexChannels :: G.Animation -> Map Int [G.Channel]
    indexChannels = foldl' insert mempty . G.animationChannels
     where
      insert :: Map Int [G.Channel] -> G.Channel -> Map Int [G.Channel]
      insert m c = M.insertWith (flip (++)) (targetNode c) (pure c) m
       where
        targetNode = fromMaybe (error "Channel node required")
                       . G.channelTargetNode

loadMeshPrimitive :: Vector Material -> G.MeshPrimitive -> IO MeshPrimitive
loadMeshPrimitive materials G.MeshPrimitive{..} = do
  -- Do some checks
  when (null meshPrimitiveIndices) . error
    $ "Mesh primitives are required to be indexed."
  when (null meshPrimitiveNormals) . error
    $ "Mesh primitive missing normals."
  when (null meshPrimitiveTangents) . error
    $ "Mesh primitive missing tangents."
  when (null meshPrimitiveTexCoords) . error
    $ "Mesh primitive missing texture co-ordinates."
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
