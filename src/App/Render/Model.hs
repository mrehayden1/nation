module App.Render.Model (
  module App.Render.Model.Model,

  Node(..),
  MeshPrimitive(..),
  Material(..),
  G.MaterialAlphaMode(..),

  Animation,
  AnimationName,
  AnimationTime,
  AnimationDuration,

  makeTransformationMatrices
) where

import Control.Applicative
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

import App.Matrix
import App.Render.Model.Model

type Animation = (AnimationName, AnimationDuration, AnimationTime)
type AnimationName = Text
type AnimationTime = Float
type AnimationDuration = Float

makeTransformationMatrices :: Model -> Maybe Animation -> Vector (M44 Float)
makeTransformationMatrices model anim =
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
          tr = transformation r t
      in tr !*! s

nodeApplyAnimation :: Maybe Animation -> Node -> Node
nodeApplyAnimation anim n@Node{..} = fromMaybe n $ do
  (name, duration, time) <- anim
  channels <- M.lookup name nodeAnimations
  let applyChannel' = applyChannel (time `mod'` duration)
        <$> G.channelSamplerInputs
        <*> G.channelSamplerOutputs
        <*> G.channelSamplerInterpolation
  return . foldl' (flip ($)) n . fmap applyChannel' $ channels
 where
  applyChannel :: Float       -- time
    -> Vector Float           -- inputs
    -> G.ChannelSamplerOutput -- outputs
    -> G.ChannelSamplerInterpolation
    -> Node
    -> Node
  applyChannel t input (G.Rotation out) i =
    let ks = V.zip input out
        v = case i of
              G.Linear      -> spherical t ks
              G.Step        -> step      t ks
              G.CubicSpline -> normalize . cubicSpline t input $ out
    in set _nodeRotation v
  applyChannel t input (G.Scale out) i =
    let ks = V.zip input out
        v = case i of
              G.Linear      -> linear t ks
              G.Step        -> step   t ks
              G.CubicSpline -> cubicSpline t input out
    in set _nodeScale v
  applyChannel t input (G.Translation out) i =
    let ks = V.zip input out
        v = case i of
              G.Linear      -> linear t ks
              G.Step        -> step   t ks
              G.CubicSpline -> cubicSpline t input out
    in set _nodeTranslation v
  -- TODO morph target weights
  applyChannel _ _     _                        _ = id

  linear :: Additive f => Float -> Vector (Float, f Float) -> f Float
  linear t ks =
    -- Use clamp first to catch the singleton keyframe case
    fromMaybe (error "nodeApplyAnimation.linear") $ clamp t ks <|> lerp' t ks

  lerp' :: Additive f => Float -> Vector (Float, f Float) -> Maybe (f Float)
  lerp' t ks = do
    (v0, v1, s) <- linearOperands t ks
    -- For some reason the imported lerp is defined with the operands switched
    return . lerp s v1 $ v0

  spherical :: Float -> Vector (Float, Quaternion Float) -> Quaternion Float
  spherical t ks =
    -- Use clamp first to catch the singleton keyframe case
    fromMaybe (error "nodeApplyAnimation.spherical")
      $ clamp t ks <|> slerp' t ks

  slerp' :: Float
    -> Vector (Float, Quaternion Float)
    -> Maybe (Quaternion Float)
  slerp' t ks = do
    (v0, v1, s) <- linearOperands t ks
    return . slerp v0 v1 $ s

  clamp :: Float -> Vector (Float, a) -> Maybe a
  clamp t ks = do
    -- Assume there's at least one frame
    -- FIXME should catch the empty vector and error here
    let (tmin, vmin) = V.head ks
        (tmax, vmax) = V.last ks
    if t <= tmin then Just vmin else Nothing
      <|> if tmax <= t then Just vmax else Nothing

  -- Get the values of the keyframe pair that time t is between and the
  -- segment-normalised interpolation factor.
  linearOperands :: Float -> Vector (Float, a) -> Maybe (a, a, Float)
  linearOperands t ks = do
    let segments = V.zip ks . V.drop 1 $ ks
    -- Assume there's at least two frames - the singleton is handled by clamp
    ((t0, v0), (t1, v1)) <-
      find ((&&) <$> ((t >=) . fst . fst) <*> ((t <) . fst . snd)) segments
    let d = t1 - t0
        s = (t - t0) / d
    return (v0, v1, s)

  step :: Float -> Vector (Float, a) -> a
  step t ks =
    let ks' = V.takeWhile ((t >=) . fst) ks
    in if null ks
         -- Ensure animation is clamped to beginning when tc < t1
         then snd . V.head $ ks
         else snd . V.last $ ks'

  cubicSpline :: Additive f
    => Float
    -> Vector Float
    -> Vector (f Float)
    -> f Float
  cubicSpline t input out =
    fromMaybe (error "nodeApplyAnimation.cubicSpline") $ do
      (v0, v1, vOut, vIn, d, s) <- cubicOperands t input out
      let v = (2 * s ** 3 - 3 * s ** 2 + 1) *^ v0
                ^+^ d * (s ** 3 + 2 * s ** 2 + s) *^ vOut
                ^+^ (3 * s ** 2 - 2 * t ** 3) *^ v1
                ^+^ d * (s ** 3 - s ** 2) *^ vIn
      return v

  -- Get the values of the keyframe pair that time t is between, the out
  -- tangent of the keyframe before, the in tangent of the next keyframe, the
  -- segment duration and the segment-normalised interpolation factor.
  cubicOperands :: Float
    -> Vector Float
    -> Vector a
    -> Maybe (a, a, a, a, Float, Float)
  cubicOperands t input out = do
    -- Triple up the outputs into in-tangents, property values and out-tangents
    let ks = V.zip input . V.unfoldr takeTriplet $ out
        segments = V.zip ks . V.drop 1 $ ks
    ((t0, (_, v0, vOut)), (t1, (vIn, v1, _))) <-
      find ((&&) <$> ((t >=) . fst . fst) <*> ((t <) . fst . snd)) segments
    let d = t1 - t0
        s = (t - t0) / d
    return (v0, v1, vOut, vIn, d, s)
   where
    takeTriplet :: Vector a -> Maybe ((a, a, a), Vector a)
    takeTriplet as =
      if length as >= 3
        then do
          (a1, as1) <- V.uncons as
          (a2, as2) <- V.uncons as1
          (a3, as3) <- V.uncons as2
          return ((a1, a2, a3), as3)
        else Nothing
