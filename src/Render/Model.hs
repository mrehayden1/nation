module Render.Model (
  Model,
  fromGlbFile,
  deleteModel,

  renderModel
) where

import Data.Foldable
import Data.Maybe
import Data.StateVar
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import qualified Text.GLTF.Loader as GLTF

data Model = Model {
  modelMeshes :: Vector MeshPrimitive
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
  eGlb <- GLTF.fromBinaryFile pathname
  gltf <- case eGlb of
    Left _  -> fail "loadModel: Failed to read GLB."
    Right g -> return $ GLTF.unGltf g
  let GLTF.Gltf{..} = gltf
  meshes <- fmap fold . mapM (mapM (loadMeshPrimitive gltfMaterials)
    . GLTF.meshPrimitives) . GLTF.gltfMeshes $ gltf
  return $ Model
    meshes

loadMeshPrimitive :: Vector GLTF.Material -> GLTF.MeshPrimitive -> IO MeshPrimitive
loadMeshPrimitive materials GLTF.MeshPrimitive{..} = do
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
  --   Position
  --   TODO replace colour with materials
  let colour = fromMaybe 1 $ getColour =<< meshPrimitiveMaterial
      colours = fmap (const colour) meshPrimitivePositions
  --   TODO Normals
  --   TODO Texture co-ordinates
  vbos <- sequence [
            loadVertexAttribute 0 3 meshPrimitivePositions,
            loadVertexAttribute 1 4 colours
          ]
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return . MeshPrimitive (toGlPrimitiveMode meshPrimitiveMode) vao vbos ebo
    . fromIntegral . length $ meshPrimitiveIndices
 where
  getColour :: Int -> Maybe (L.V4 Float)
  getColour n = do
    let material = materials ! n
    pbr <- GLTF.materialPbrMetallicRoughness material
    return . GLTF.pbrBaseColorFactor $ pbr

  loadVertexAttribute n components attrs = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    S.unsafeWith (V.convert attrs) $ \ptr -> do
      let size = fromIntegral . (* sizeOf (undefined :: Float))
                   . (* components) . length $ attrs
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    let attrLoc = GL.AttribLocation n
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

toGlPrimitiveMode :: GLTF.MeshPrimitiveMode -> GL.PrimitiveMode
toGlPrimitiveMode p = case p of
  GLTF.Lines         -> GL.Lines
  GLTF.LineLoop      -> GL.LineLoop
  GLTF.LineStrip     -> GL.LineStrip
  GLTF.Points        -> GL.Points
  GLTF.Triangles     -> GL.Triangles
  GLTF.TriangleFan   -> GL.TriangleFan
  GLTF.TriangleStrip -> GL.TriangleStrip

deleteModel :: Model -> IO ()
deleteModel = undefined

-- Renders a model in the current GL pipeline
renderModel :: Model -> IO ()
renderModel Model{..} = do
  mapM_ renderMesh modelMeshes
 where
  renderMesh :: MeshPrimitive -> IO ()
  renderMesh MeshPrimitive{..} = do
    GL.bindVertexArrayObject $= Just meshVao
    GL.drawElements meshGlPrimitiveMode meshNumIndices GL.UnsignedShort nullPtr
    GL.bindVertexArrayObject $= Nothing
