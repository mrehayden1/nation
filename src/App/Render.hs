module App.Render (
  module App.Render.Env,

  createRenderer
) where

import Control.Applicative
import Control.Monad
import Control.Parallel.Strategies
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear

import App.Collision
import App.Collision.AABB
import App.Collision.BVH
import App.Entity
import App.Game hiding (Env)
import App.Map
import App.Matrix
import App.Projection
import qualified App.Quaternion as Q
import App.Render.Debug
import App.Render.Env
import App.Render.Model
import App.Render.Scene as Scene
import App.Render.Scene.Entity
import App.Render.Scene.Shadow
import App.Render.UI
import App.Vector

createRenderer :: IORef POSIXTime
  -> IO (BVH MapTree (V3 Float) -> Frame -> Render ())
createRenderer timeRef = do
  (shadowMapTexture, renderShadowMap) <- createShadowMapper
  renderScene <- createSceneRenderer shadowMapTexture
  renderUi <- createUiRenderer
  overlayConsole <- createConsoleOverlayer
  overlayDebugQuad <- createDebugQuadOverlayer shadowMapTexture
  overlayDebugInfo <- createDebugInfoOverlayer timeRef
  overlayGizmo <- createDebugGizmoOverlayer
  -- TODO Move this lambda function to a definition
  -- React to changes in our Reflex application
  return $ \treesBVH frame@(_, Output{..}) -> do
    let World{..} = outputWorld
    scene <- cullScene =<< makeScene treesBVH frame
    renderShadowMap scene
    renderScene scene
    renderUi frame
    when outputDebugQuadOverlay overlayDebugQuad
    when outputDebugInfoOverlay $ do
      overlayDebugInfo frame
      unless outputDebugQuadOverlay . overlayGizmo $ worldCamera
    when outputConsoleOpen $ overlayConsole frame

makeScene :: BVH MapTree (V3 Float) -> Frame -> Render Scene
makeScene treeBVH (_, Output{..}) = do
  aspectRatio <- asks viewportAspectRatio
  let World{..} = outputWorld
      App.Game.Daylight{..} = worldDaylight
      -- Roughly cull static map data using our BVH of AABBs and the camera
      -- frustum AABB.
      cameraAabb = aabb . cameraFrustum aspectRatio $ worldCamera
      trees = query (intersecting cameraAabb) treeBVH
  treeInstances <- mapM treeInstance trees
  peasantInstances <- mapM (peasantInstance worldAnimationTime) worldPeasants
  coinInstances <- mapM (coinInstance worldAnimationTime) worldCoins
  playerInstances <- fmap pure
    . playerInstance worldAnimationTime worldPlayerVelocity
                     worldPlayerDirection
    $ worldPlayerPosition
  grassModel <- asks (entityModel EntityGrass . envModels)
  pointerModel <- asks (entityModel EntityPointer . envModels)
  return $ Scene {
    sceneCamera = worldCamera,
    sceneDaylight = Scene.Daylight {
      daylightColor = daylightColor,
      daylightIntensity = daylightIntensity,
      daylightPitch = daylightPitch,
      daylightYaw = daylightYaw
    },
    sceneEntities = M.fromList [
      -- Grass
      (EntityGrass, [makeInstance grassModel Q.identity 0 Nothing]),
      -- Trees
      (EntityOakTree, treeInstances),
      -- Pointer
      (EntityPointer, [
        makeInstance pointerModel Q.identity worldPointerPosition
          . Just $ ("spin", 5, worldAnimationTime)
      ]),
      -- Peasants
      (EntityPeasant, peasantInstances),
      -- Coins
      (EntityCoin, coinInstances),
      -- Player
      (EntityPlayer, playerInstances)
    ]
  }

playerInstance :: Float -> V3 Float -> V3 Float -> V3 Float -> Render Instance
playerInstance t v d p = do
  let animation =
        if quadrance v > 0
          then Just ("Gallop", 0.67, t)
          else Just ("Idle2" , 6   , t)
      r = Q.fromVectors (V3 1 0 0) d
  model <- asks (entityModel EntityPlayer . envModels)
  return . makeInstance model r p $ animation

coinInstance :: Float -> Coin -> Render Instance
coinInstance t (Coin _ p) = do
  model <- asks (entityModel EntityCoin . envModels)
  return . makeInstance model Q.identity p . Just $ ("Spin", 2.08, t)

peasantInstance :: Float -> Peasant -> Render Instance
peasantInstance t (Peasant d p v) = do
  let animation =
        if quadrance v > 0
          then Just ("Walk"     , 0.66, t)
          else Just ("Idle Long", 4   , t)
      rotation = Q.fromVectors (V3 1 0 0) d
  model <- asks (entityModel EntityPeasant . envModels)
  return . makeInstance model rotation p $ animation

treeInstance :: MapTree -> Render Instance
treeInstance MapTree{..} = do
  let V2 x z = mapTreePosition
      p = V3 x 0 z
      rotation = Q.fromVectors (V3 1 0 0) . eulerDirection 0 $ mapTreeRotation
  model <- asks (entityModel EntityOakTree . envModels)
  return . makeInstance model rotation p $ Nothing

makeInstance :: Model -> Quaternion Float -> V3 Float -> Maybe Animation -> Instance
makeInstance model@Model{..} rotation translation' animation =
  let modelMatrix = transformation rotation translation'
      transformationMatrices = makeTransformationMatrices model animation
      jointMatrices = fmap (makeJointMatrices transformationMatrices) modelSkins
  in Instance {
       instanceModelMatrix = modelMatrix,
       instanceTransformationMatrices = transformationMatrices,
       instanceJointMatrices = jointMatrices
     }
 where
  -- The renderer expects a joint matrix for every node regardless of wether it
  -- has skinned meshes or not, so make joint matrices for each node if it has a skin, otherwise default to
  -- identity
  makeJointMatrices :: Vector (M44 Float) -> Skin -> Vector (M44 Float)
  makeJointMatrices transformations skin =
    let ts       = fmap (transformations V.!) . skinJoints $ skin
        invBinds = skinInverseBindMatrices skin
    in V.zipWith (!*!) ts invBinds

-- Cull entities in the scene by the camera frustum and their frustum culling
-- volume, if defined.
cullScene :: Scene -> Render Scene
cullScene scene@Scene{..} = do
  aspectRatio <- asks viewportAspectRatio
  let frustum' = frustumCollision . cameraFrustum aspectRatio $ sceneCamera
      culledEntities = M.mapWithKey (cullInstances frustum') sceneEntities
      scene' = scene { sceneEntities = culledEntities }
  return scene'
 where
  frustumCollision :: (Epsilon a, Floating a) => Frustum a -> Collision3 a
  frustumCollision =
    liftA2 CollisionPolyhedron frustumPoints frustumFaceNormals

  cullInstances :: Collision3 Float -> Entity -> [Instance] -> [Instance]
  cullInstances frustum' entity = filter shouldRender
   where
    shouldRender :: Instance -> Bool
    shouldRender Instance{..} =
      maybe True
            (collided3 frustum' . transformCollision3 instanceModelMatrix)
        . entityCullingBounds $ entity

  parFilter :: (a -> Bool) -> [a] -> [a]
  parFilter f = catMaybes . runEval . parListChunk 200 rseq . map f'
   where
    f' a = if f a then Just a else Nothing
