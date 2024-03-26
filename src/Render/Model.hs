module Render.Model (
  module Render.Model.Model,
  module Render.Model.GLTF,

  Node(..),
  MeshPrimitive(..),
  Material(..),
  G.MaterialAlphaMode(..),

  makeGlobalTransforms,
  makeGlobalTransforms',

  makeJointMatrices
) where

import Control.Lens hiding (transform)
import Data.Fixed
import Data.Foldable as F
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear
import qualified Text.GLTF.Loader as G

import Matrix
import Render.Model.Model
import Render.Model.GLTF

type Animation = (Text, Float, Float) -- Animation name, duration and time

{-
withRenderer :: Renderer
  -> Maybe Animation
  -> V3 Float
  -> Quaternion Float
  -> Model
  -> Render ()
withRenderer render anim pos rot model =
  let Model{..} = model
  in V.zipWithM_ nodeWithRenderer globalTransforms modelNodes
 where
  modelMatrix :: M44 Float
  modelMatrix = mkTransformation rot pos

  nodeWithRenderer :: M44 Float -> Node -> Render ()
  nodeWithRenderer globalTransform node = do
    Env{..} <- ask
    let jointMatrices = maybe mempty makeJointMatrices nodeSkin
    mapM_ (mapM_ (render modelMatrix globalTransform jointMatrices)) nodeMesh
-}

makeGlobalTransforms :: Model -> Maybe Animation -> Vector (M44 Float)
makeGlobalTransforms = makeGlobalTransforms' False

makeGlobalTransforms' :: Bool -> Model -> Maybe Animation -> Vector (M44 Float)
makeGlobalTransforms' translateOnly model anim =
  let Model{..} = model
  in ((modelNodes $> identity) V.//)
       . concatMap (globalTransforms' modelNodes identity)
       $ modelScene
 where
  globalTransforms' :: Vector Node
    -> M44 Float
    -> Int
    -> [(Int, M44 Float)]
  globalTransforms' nodes m i =
    let n = nodes V.! i
        m' = (m !*!) . nodeLocalTransform . nodeApplyAnimation anim $ n
        ts = concatMap (globalTransforms' nodes m')
               . nodeChildren $ n
    in (i, m') : ts
   where
    nodeLocalTransform :: Node -> M44 Float
    nodeLocalTransform Node{..} =
      let s = scale nodeScale
          r = nodeRotation
          t = nodeTranslation
          tr = mkTransformation r t
      in if translateOnly then translate t else tr !*! s

nodeApplyAnimation :: Maybe Animation -> Node -> Node
nodeApplyAnimation anim n@Node{..} = fromMaybe n $ do
  (name, duration, time) <- anim
  channels <- M.lookup name nodeAnimations
  let applyChannel' = applyChannel (time `mod'` duration)
        <$> G.channelSamplerInputs
        <*> G.channelSamplerOutputs
  return . foldl' (flip ($)) n . fmap applyChannel' $ channels
 where
  applyChannel :: Float       -- time
    -> Vector Float           -- inputs
    -> G.ChannelSamplerOutput -- outputs
    -> Node
    -> Node
  applyChannel t input (G.Rotation rs) =
    maybe id (set _nodeRotation) . interpolate t . V.zip input $ rs
  applyChannel t input (G.Scale ss) =
    maybe id (set _nodeScale) . interpolate t . V.zip input $ ss
  applyChannel t input (G.Translation ts) =
    maybe id (set _nodeTranslation) . interpolate t . V.zip input $ ts
  applyChannel _ _ _ = id

  -- FIXME Do the interpolation requested by the channel
  interpolate :: Float -> Vector (Float, a) -> Maybe a
  interpolate t keyframes = do
    i <- V.findIndex ((> t) . fst) keyframes
    fmap snd . (keyframes V.!?) $ i

makeJointMatrices :: Vector (M44 Float) -> Skin -> Vector (M44 Float)
makeJointMatrices globalTransforms skin =
  let ts       = fmap (globalTransforms V.!) . skinJoints $ skin
      invBinds = skinInverseBindMatrices skin
  in V.zipWith (!*!) ts invBinds
