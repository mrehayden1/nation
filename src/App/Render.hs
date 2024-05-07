module App.Render (
  module App.Render.Env,

  createRenderer
) where

import Control.Monad
import Data.IORef
import Data.Time.Clock.POSIX
import Linear

import App.Entity
import App.Entity.Collision3D
import App.Game hiding (Env)
import App.Matrix
import App.Quaternion as Q
import App.Render.Debug
import App.Render.Env
import App.Render.Scene as Scene
import App.Render.Scene.Shadow
import App.Render.UI

createRenderer :: IORef POSIXTime
                    -> Entities
                    -> IO (Frame -> Render ())
createRenderer timeRef entities = do
  (shadowMapTexture, renderShadowMap) <- createShadowMapper
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
    scene <- cullScene . makeScene entities $ frame
    renderShadowMap scene
    renderScene scene
    renderUi frame
    when outputDebugQuadOverlay overlayDebugQuad
    when outputDebugInfoOverlay $ do
      overlayDebugInfo frame
      unless outputDebugQuadOverlay . overlayGizmo $ worldCamera
    when outputConsoleOpen $ overlayConsole frame

makeScene :: Entities -> Frame -> Scene
makeScene Entities{..} (_, Output{..}) =
  let World{..} = outputWorld
      App.Game.Daylight{..} = worldDaylight
      trees = fmap (makeTree entitiesOakTree)
                   [V3 15 0 15]
                   --[V3 (x*15) 0 (y*15) | x <- [-15..15], y <- [-15..15]]
                   --[V3 (x*15) 0 (y*15) | x <- [-124..125], y <- [-124..125]]
      peasants = fmap (makePeasant entitiesPeasant worldAnimationTime) worldPeasants
      coins = fmap (makeCoin entitiesCoin worldAnimationTime) worldCoins
  in Scene {
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
  makeCoin :: CoinE -> Float -> Coin -> Element
  makeCoin coinE t (Coin _ p) =
    Element {
      elementAnimation = Just ("Spin", 2.08, t),
      elementCullingBounds = Nothing,
      elementModel = coinEModel coinE,
      elementPosition = p,
      elementRotation = Q.identity,
      elementShadow = True
    }

  makePeasant :: PeasantE -> Float -> Peasant -> Element
  makePeasant peasantE t (Peasant d p v) =
    Element {
      elementAnimation = if quadrance v > 0
        then Just ("Walk"     , 0.66, t)
        else Just ("Idle Long", 4   , t),
      elementCullingBounds = Nothing,
      elementModel = peasantEModel peasantE,
      elementPosition = p,
      elementRotation = Q.fromVectors (V3 1 0 0) d,
      elementShadow = True
    }

  makeTree :: OakTreeE -> V3 Float -> Element
  makeTree oakTreeE p =
    Element {
      elementAnimation = Nothing,
      elementCullingBounds = Just . transformCollision3d (translate p)
        . oakTreeECullingBounds $ oakTreeE,
      elementModel = oakTreeEModel oakTreeE,
      elementPosition = p,
      elementRotation = Q.identity,
      elementShadow = True
    }
