module Render.Model (
  Model,
  fromGlbFile,
  deleteModel,

  renderModel
) where

import Data.Foldable
import Data.Maybe
import qualified Data.Set as Set
import Data.StateVar
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear (M44, V3(..), V4(..), (!*!))
import qualified Linear as L
import Text.GLTF.Loader (Gltf(..))
import qualified Text.GLTF.Loader as G

import Render.Matrix as M
import Render.Pipeline

newtype Model = Model {
  -- A mesh is a list of mesh primitives and their local model matrices
  modelMeshes :: Vector (L.M44 Float, Vector MeshPrimitive)
}

-- Renderable mesh components
data MeshPrimitive = MeshPrimitive {
  meshGlPrimitiveMode :: GL.PrimitiveMode,
  meshVao :: GL.VertexArrayObject,
  meshVbo :: [GL.BufferObject],
  meshEbo :: GL.BufferObject,
  meshNumIndices :: GL.NumArrayIndices
}

fromGlbFile :: FilePath -> IO Model
fromGlbFile pathname = do
  eGlb <- G.fromBinaryFile pathname
  gltf <- case eGlb of
    Left  _ -> fail "loadModel: Failed to read GLB."
    Right g -> return $ G.unGltf g
  let Gltf{..} = gltf
  -- TODO Traverse the scene graph and apply transformations
  meshes <- createMeshes gltfMeshes gltfNodes
  return $ Model meshes
 where
  createMeshes :: Vector G.Mesh
    -> Vector G.Node
    -> IO (Vector (L.M44 Float, Vector MeshPrimitive))
  createMeshes meshes nodes =
    let childrenIx = Set.fromList . toList . foldMap G.nodeChildren $ nodes
        roots = V.ifilter (\i _ -> i `Set.notMember` childrenIx) nodes
    in fmap V.fromList . mapM (\(t, m) -> (t,) <$> mapM loadMeshPrimitive m)
         . concatMap (accum L.identity) $ roots
   where
    accum :: L.M44 Float
      -> G.Node
      -> [(L.M44 Float, Vector G.MeshPrimitive)]
    accum t G.Node{..} =
      let s = maybe L.identity scale nodeScale
          tr = fromMaybe L.identity $
                 L.mkTransformation <$> nodeRotation <*> nodeTranslation
          str = tr !*! s
          t' = str !*! t
          mesh = (t',) . maybe mempty (G.meshPrimitives . (meshes !))
                   $ nodeMeshId
          children = fmap (nodes !) nodeChildren
          childMeshes = concatMap (accum t') children
      in mesh : childMeshes
     where
      scale :: Num a => V3 a -> M44 a
      scale (V3 x y z) = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)

loadMeshPrimitive :: G.MeshPrimitive -> IO MeshPrimitive
loadMeshPrimitive G.MeshPrimitive{..} = do
  -- Create and bind VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Load indices
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  S.unsafeWith (V.convert meshPrimitiveIndices) $ \ptr -> do
    let size = fromIntegral . (* sizeOf (undefined :: Word16)) . length
                 $ meshPrimitiveIndices
    GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Load vertex data
  vbos <- sequence [
      -- Position
      loadVertexAttribute 0 3 meshPrimitivePositions,
      -- Normals
      loadVertexAttribute 1 3 meshPrimitiveNormals
      -- Texture co-ordinates
    ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return . MeshPrimitive (toGlPrimitiveMode meshPrimitiveMode) vao vbos ebo
    . fromIntegral . length $ meshPrimitiveIndices
 where
  loadVertexAttribute location components attrs = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    S.unsafeWith (V.convert attrs) $ \ptr -> do
      let size = fromIntegral . (* sizeOf (undefined :: Float))
                   . (* components) . length $ attrs
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    let attrLoc = GL.AttribLocation location
    GL.vertexAttribPointer attrLoc $=
      (GL.ToFloat,
       GL.VertexArrayDescriptor
         (fromIntegral components)
         GL.Float
         0 -- stride=0, only one attribute
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

-- Renders a model in the current GL pipeline
renderModel :: Pipeline -> Model -> IO ()
renderModel pipeline Model{..} = do
  mapM_ (uncurry renderMesh) modelMeshes
 where
  renderMesh :: L.M44 Float -> Vector MeshPrimitive -> IO ()
  renderMesh modelMatrix = mapM_ (renderMeshPrimitive modelMatrix)

  renderMeshPrimitive :: L.M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive modelMatrix' MeshPrimitive{..} = do
    -- Set model matrix
    modelMatrix <- M.toGlMatrix modelMatrix'
    let modelMatrixUniform = pipelineUniform pipeline "modelM"
    modelMatrixUniform $= (modelMatrix :: GL.GLmatrix GL.GLfloat)
    GL.bindVertexArrayObject $= Just meshVao
    GL.drawElements meshGlPrimitiveMode meshNumIndices GL.UnsignedShort nullPtr
    GL.bindVertexArrayObject $= Nothing
