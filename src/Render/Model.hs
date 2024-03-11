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
import Data.Fixed
import Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear
import qualified Text.GLTF.Loader as G

import Matrix
import Render.Model.Model
import Render.Model.GLTF

import Debug.Trace

withRenderer :: forall m. Monad m
  => (M44 Float -> MeshPrimitive -> m ())
  -> V3 Float
  -> Quaternion Float
  -> Model
  -> m ()
withRenderer render = withRendererPosed render Nothing

withRendererPosed :: Monad m
  => (M44 Float -> MeshPrimitive -> m ())
  -> Maybe (Text, Float, Float) -- Animation name and duration and, time
  -> V3 Float
  -> Quaternion Float
  -> Model
  -> m ()
withRendererPosed render anim pos rot =
  traverse_ (uncurry $ nodeWithRenderer render)
    . accumTransforms (mkTransformation rot pos) anim . _modelScene
 where
  nodeWithRenderer :: Monad m
    => (M44 Float -> MeshPrimitive -> m ())
    -> M44 Float
    -> SceneNode
    -> m ()
  nodeWithRenderer render' m SceneNode{..} =
    mapM_ (mapM_ (render' m)) _nodeMesh

accumTransforms :: M44 Float
  -> Maybe (Text, Float, Float)
  -> Tree SceneNode
  -> Tree (M44 Float, SceneNode)
accumTransforms m anim (Node n ns) =
  let m' = (m !*!) . nodeTransformation . applyAnimation anim $ n
  in Node (m', n) . fmap (accumTransforms m' anim) $ ns

nodeTransformation :: SceneNode -> M44 Float
nodeTransformation SceneNode{..} =
  let s = scale _nodeScale
      r = _nodeRotation
      t = _nodeTranslation
      tr = mkTransformation r t
  in tr !*! s

applyAnimation :: Maybe (Text, Float, Float) -> SceneNode -> SceneNode
applyAnimation anim n@SceneNode{..} = fromMaybe n $ do
  (name, duration, time) <- anim
  channels <- M.lookup name _nodeAnimations
  return . foldl' (flip ($)) n
    . fmap (applyChannel (time `mod'` duration) <$> G.channelSamplerInputs <*> G.channelSamplerOutputs) $ channels
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
