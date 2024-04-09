module Render (
  module Render.Env,

  createRenderer
) where

import Control.Monad
import Data.IORef
import Data.Time.Clock.POSIX
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App hiding (Env)
import Entity
import Quaternion as Q
import Render.Debug
import Render.Env
import Render.Scene as Scene
import Render.Scene.Shadow
import Render.UI
import Vector

createRenderer :: GLFW.Window
                    -> IORef POSIXTime
                    -> Entities
                    -> IO (Frame -> Render ())
createRenderer win timeRef Entities{..} = do
  -- Create a depth buffer object and depth map texture
  (shadowMapTexture, renderShadowMap)
    <- createShadowMapper
  renderScene <- createSceneRenderer shadowMapTexture
  renderUi <- createUiRenderer
  overlayConsole <- createConsoleOverlayer
  overlayDebugQuad <- createDebugQuadOverlayer shadowMapTexture
  overlayDebugInfo <- createDebugInfoOverlayer timeRef
  overlayGizmo <- createDebugGizmoOverlayer
  -- TODO Move this to it's own definition
  -- React to changes in our Reflex application
  return $ \frame@(_, Output{..}) -> do
    let World{..} = outputWorld
        scene = makeScene frame
    renderShadowMap scene
    renderScene scene
    renderUi frame
    when outputDebugQuadOverlay overlayDebugQuad
    when outputDebugInfoOverlay $ do
      overlayDebugInfo frame
      unless outputDebugQuadOverlay . overlayGizmo $ worldCamera
    when outputConsoleOpen $ overlayConsole frame
    liftIO $ GLFW.swapBuffers win
 where
  makeScene (_, Output{..}) =
    let World{..} = outputWorld
        App.Daylight{..} = worldDaylight
    in Scene {
      sceneCamera = worldCamera,
      sceneElements = [
        -- Grass
        Element {
          elementAnimation = Nothing,
          elementModel = grassEModel entitiesGrass,
          elementPosition = 0,
          elementRotation = Q.identity,
          elementShadow = True
        },
        -- Pointer
        Element {
          elementAnimation = Just ("spin", 5, worldAnimationTime),
          elementModel = pointerEModel entitiesPointer,
          elementPosition = worldPointerPosition,
          elementRotation = Q.identity,
          elementShadow = False
        }
        -- Peasant
      ] ++ fmap (mkPeasant worldAnimationTime) worldPeasants
        -- Coins
        ++ fmap (mkCoin worldAnimationTime) worldCoins ++ [
        -- Horse
        Element {
          elementAnimation = if magnitude2 worldPlayerVelocity > 0
            then Just ("Gallop", 0.67, worldAnimationTime)
            else Just ("Idle2" , 6   , worldAnimationTime),
          elementModel = playerEModel entitiesPlayer,
          elementPosition = worldPlayerPosition,
          elementRotation = Q.fromVectors (V3 1 0 0) worldPlayerDirection,
          elementShadow = True
        }
      ],
      sceneDaylight = Scene.Daylight {
        daylightAmbientIntensity = daylightAmbientIntensity,
        daylightPitch = daylightPitch,
        daylightYaw = daylightYaw
      }
    }
   where
    mkCoin t (Coin _ p) =
      Element {
        elementAnimation = Just ("Spin", 2.08, t),
        elementModel = coinEModel entitiesCoin,
        elementPosition = p,
        elementRotation = Q.identity,
        elementShadow = True
      }

    mkPeasant t (Peasant d p v) =
      Element {
        elementAnimation = if magnitude v > 0
          then Just ("Walk"     , 0.66, t)
          else Just ("Idle Long", 4   , t),
        elementModel = peasantEModel entitiesPeasant,
        elementPosition = p,
        elementRotation = Q.fromVectors (V3 1 0 0) d,
        elementShadow = True
      }
