module App.Render (
  module App.Render.Env,

  createRenderer
) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Time.Clock.POSIX
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Camera
import App.Entity
import App.Entity.Collision3D
import App.Game hiding (Env)
import App.Matrix
import App.Projection
import App.Quaternion as Q
import App.Render.Debug
import App.Render.Env
import App.Render.Scene as Scene
import App.Render.Scene.Shadow
import App.Render.UI

createRenderer :: GLFW.Window
                    -> IORef POSIXTime
                    -> IO (Frame -> Render ())
createRenderer win timeRef = do
  -- Create a depth buffer object and depth map texture
  (shadowMapTexture, renderShadowMap)
    <- createShadowMapper
  renderScene <- createSceneRenderer shadowMapTexture
  renderUi <- createUiRenderer
  overlayConsole <- createConsoleOverlayer
  overlayDebugQuad <- createDebugQuadOverlayer shadowMapTexture
  overlayDebugInfo <- createDebugInfoOverlayer timeRef
  overlayGizmo <- createDebugGizmoOverlayer
  -- TODO Move this lambda function to a definition
  -- React to changes in our Reflex application
  return $ \frame@(_, Output{..}) -> do
    let World{..} = outputWorld
    scene <- cullScene worldCamera =<< makeScene frame
    renderShadowMap scene
    renderScene scene
    renderUi frame
    when outputDebugQuadOverlay overlayDebugQuad
    when outputDebugInfoOverlay $ do
      overlayDebugInfo frame
      unless outputDebugQuadOverlay . overlayGizmo $ worldCamera
    when outputConsoleOpen $ overlayConsole frame
    liftIO $ GLFW.swapBuffers win

cullScene :: Camera Float -> Scene -> Render Scene
cullScene camera scene = do
  aspectRatio <- asks viewportAspectRatio
  let collision = frustumCollision . cameraFrustum aspectRatio $ camera
      culledElements =
        filter (maybe True (collided3d collision) . elementCullingBounds)
          . sceneElements $ scene
      scene' = scene { sceneElements = culledElements }
  return scene'
 where
  frustumCollision :: (Epsilon a, Floating a) => Frustum a -> Collision3D a
  frustumCollision =
    liftA2 CollisionPolyhedron frustumPoints frustumFaceNormals

makeScene :: Frame -> Render Scene
makeScene (_, Output{..}) = do
  let World{..} = outputWorld
      App.Game.Daylight{..} = worldDaylight
  Entities{..} <- asks renderEnvEntities
  --trees <- mapM makeTree [V3 (x*15) 0 (y*15) | x <- [-124..125], y <- [-124..125]]
  trees <- mapM makeTree [V3 (x*15) 0 (y*15) | x <- [-15..15], y <- [-15..15]]
  peasants <- mapM (makePeasant worldAnimationTime) worldPeasants
  coins <- mapM (makeCoin worldAnimationTime) worldCoins
  return $ Scene {
    sceneCamera = worldCamera,
    sceneElements = concat [
      -- Grass
      [
        Element {
          elementAnimation = Nothing,
          elementCullingBounds = Nothing,
          elementModel = grassEModel entitiesGrass,
          elementPosition = 0,
          elementRotation = Q.identity,
          elementShadow = True
        }
      ],
      trees,
      -- Pointer
      [
        Element {
          elementAnimation = Just ("spin", 5, worldAnimationTime),
          elementCullingBounds = Nothing,
          elementModel = pointerEModel entitiesPointer,
          elementPosition = worldPointerPosition,
          elementRotation = Q.identity,
          elementShadow = False
        }
      ],
      peasants,
      coins,
      -- Horse
      [
        Element {
          elementAnimation = if quadrance worldPlayerVelocity > 0
            then Just ("Gallop", 0.67, worldAnimationTime)
            else Just ("Idle2" , 6   , worldAnimationTime),
          elementCullingBounds = Nothing,
          elementModel = playerEModel entitiesPlayer,
          elementPosition = worldPlayerPosition,
          elementRotation = Q.fromVectors (V3 1 0 0) worldPlayerDirection,
          elementShadow = True
        }
      ]
    ],
    sceneDaylight = Scene.Daylight {
      daylightAmbientIntensity = daylightAmbientIntensity,
      daylightPitch = daylightPitch,
      daylightYaw = daylightYaw
    }
  }
 where
  makeCoin :: Float -> Coin -> Render Element
  makeCoin t (Coin _ p) = do
    Entities{..} <- asks renderEnvEntities
    return $ Element {
      elementAnimation = Just ("Spin", 2.08, t),
      elementCullingBounds = Nothing,
      elementModel = coinEModel entitiesCoin,
      elementPosition = p,
      elementRotation = Q.identity,
      elementShadow = True
    }

  makePeasant :: Float -> Peasant -> Render Element
  makePeasant t (Peasant d p v) = do
    Entities{..} <- asks renderEnvEntities
    return $ Element {
      elementAnimation = if quadrance v > 0
        then Just ("Walk"     , 0.66, t)
        else Just ("Idle Long", 4   , t),
      elementCullingBounds = Nothing,
      elementModel = peasantEModel entitiesPeasant,
      elementPosition = p,
      elementRotation = Q.fromVectors (V3 1 0 0) d,
      elementShadow = True
    }

  makeTree :: V3 Float -> Render Element
  makeTree p = do
    Entities{..} <- asks renderEnvEntities
    return $ Element {
      elementAnimation = Nothing,
      elementCullingBounds = Just . transformCollision3d (translate p)
        . oakTreeECullingBounds $ entitiesOakTree,
      elementModel = oakTreeEModel entitiesOakTree,
      elementPosition = p,
      elementRotation = Q.identity,
      elementShadow = True
    }
