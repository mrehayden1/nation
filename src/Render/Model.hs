module Render.Model (
  module Render.Model.Model,
  module Render.Model.GLTF,

  SceneNode(..),
  MeshPrimitive(..),
  Material(..),
  G.MaterialAlphaMode(..),

  withRenderer,
  withRendererPosed,

  deleteModel
) where

import Control.Lens hiding (transform)
import Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear ((!*!))
import qualified Linear as L
import qualified Text.GLTF.Loader as G

import Render.Matrix as M
import Render.Model.Model
import Render.Model.GLTF

withRenderer :: forall m. Monad m
  => (L.M44 Float -> MeshPrimitive -> m ())
  -> Model
  -> m ()
withRenderer render = withRendererPosed render Nothing

withRendererPosed :: Monad m
  => (L.M44 Float -> MeshPrimitive -> m ())
  -> Maybe (Text, Float) -- Animation name and time
  -> Model
  -> m ()
withRendererPosed render anim = traverse_ (nodeWithRenderer render)
  . accumTransforms L.identity . _modelScene
 where
  nodeWithRenderer :: Monad m
    => (L.M44 Float -> MeshPrimitive -> m ())
    -> (L.M44 Float, SceneNode)
    -> m ()
  nodeWithRenderer render' (m, SceneNode{..}) =
    mapM_ (mapM_ (render' m)) _nodeMesh

  accumTransforms :: L.M44 Float
    -> Tree SceneNode
    -> Tree (L.M44 Float, SceneNode)
  accumTransforms m (Node n ns) =
    let m' = (m !*!) . nodeTransform . applyAnimation $ n
    in Node (m', n) . fmap (accumTransforms m') $ ns

  nodeTransform :: SceneNode -> L.M44 Float
  nodeTransform SceneNode{..} =
    let s = M.scale _nodeScale
        r = L.fromQuaternion _nodeRotation
        t = _nodeTranslation
        tr = L.mkTransformationMat r t
    in tr !*! s

  applyAnimation :: SceneNode -> SceneNode
  applyAnimation n@SceneNode{..} = fromMaybe n $ do
    (name, t) <- anim
    channels <- M.lookup name _nodeAnimations
    return . foldl' (flip ($)) n
      . fmap (applyChannel t <$> G.channelSamplerInputs <*> G.channelSamplerOutputs) $ channels
   where
    applyChannel :: Float
      -> Vector Float
      -> G.ChannelSamplerOutput
      -> SceneNode
      -> SceneNode
    applyChannel t input (G.Rotation rs) =
      maybe id (set nodeRotation) . interpolate t . V.zip input $ rs
    applyChannel t input (G.Scale ss) =
      maybe id (set nodeScale) . interpolate t . V.zip input $ ss
    applyChannel t input (G.Translation ts) =
      maybe id (set nodeTranslation) . interpolate t . V.zip input $ ts
    applyChannel _ _ _ = id

    interpolate :: Float -> Vector (Float, a) -> Maybe a
    interpolate t keyframes = do
      i <- V.findIndex ((> t) . fst) keyframes
      fmap snd . (keyframes V.!?) $ i

deleteModel :: Model -> IO ()
deleteModel = undefined
