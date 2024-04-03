module Render.Model.GLTF (
  fromGlbFile
) where

import Control.Monad
import Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as GL
import Linear
import Text.GLTF.Loader (Gltf(..))
import qualified Text.GLTF.Loader as G

import qualified Render.Model.GLTF.Material as M
import Render.Model.GLTF.Texture as GT
import Render.Model.Model

fromGlbFile :: FilePath -> IO Model
fromGlbFile pathname = do
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
  meshes <- mapM (mapM (loadMeshPrimitive materials) . G.meshPrimitives)
              gltfMeshes
  let nodes = makeNodes gltfNodes gltfAnimations gltfSkins meshes
      -- FIXME Because we don't have a top level `scene` property yet, default
      -- to the first scene in the list.
      scene = G.sceneNodes . V.head $ gltfScenes
  return $ Model nodes scene gltfSkins 0
 where
  getMaterialBaseColorTextureId = pure . G.textureId <=< G.pbrBaseColorTexture
    <=< G.materialPbrMetallicRoughness

  -- TODO Decompose transformation matrices when given instead of TRS
  -- properties.
  makeNodes :: Vector G.Node
    -> Vector G.Animation
    -> Vector Skin
    -> Vector Mesh
    -> Vector Node
  makeNodes nodes animations skins meshes =
    V.imap makeSceneNode nodes
   where
    jointNodes :: Vector Int
    jointNodes = foldl' (flip $ (V.++) . skinJoints) mempty skins

    animMap :: Map Text (Map Int [G.Channel])
    animMap = indexAnimations . toList $ animations

    makeSceneNode :: Int -> G.Node-> Node
    makeSceneNode i node =
      let anims = M.mapMaybe (M.lookup i) animMap
          children = G.nodeChildren node
          mesh = fmap (meshes !) . G.nodeMeshId $ node
          rot = fromMaybe (Quaternion 1 0) . G.nodeRotation $ node
          scale = fromMaybe 1 . G.nodeScale $ node
          skin = G.nodeSkin node
          trans = fromMaybe 0 . G.nodeTranslation $ node
          isJoint = V.elem i jointNodes
          --inverseBind = inv44 $ mkTransformation r t !*! scale s
      in Node anims children isJoint mesh rot scale skin trans

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
  -- Do some checks for optional data that we need to be included in a GlTF/GLB
  -- export to be able to render it.
  when (null meshPrimitiveIndices) . error
    $ "Mesh primitives are required to be indexed."
  when (null meshPrimitiveNormals) . error
    $ "Mesh primitive missing required normals."
  when (null meshPrimitiveTangents) . error
    $ "Mesh primitive missing required tangents."
  when (null meshPrimitiveTexCoords) . error
    $ "Mesh primitive missing required texture co-ordinates."
  let mode = toGlPrimitiveMode meshPrimitiveMode
      material = maybe defaultMaterial (materials !) meshPrimitiveMaterial
  meshPrimitive material mode meshPrimitiveIndices meshPrimitivePositions
    meshPrimitiveNormals meshPrimitiveTangents meshPrimitiveTexCoords
    meshPrimitiveJoints meshPrimitiveWeights

toGlPrimitiveMode :: G.MeshPrimitiveMode -> GL.PrimitiveMode
toGlPrimitiveMode p = case p of
  G.Lines         -> GL.Lines
  G.LineLoop      -> GL.LineLoop
  G.LineStrip     -> GL.LineStrip
  G.Points        -> GL.Points
  G.Triangles     -> GL.Triangles
  G.TriangleFan   -> GL.TriangleFan
  G.TriangleStrip -> GL.TriangleStrip
