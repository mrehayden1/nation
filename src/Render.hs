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
import Model
import Render.Debug
import Render.Env
import Render.Scene as Scene
import Render.Scene.Shadow

createRenderer :: GLFW.Window
                    -> IORef POSIXTime
                    -> Models
                    -> IO (Frame -> Render ())
createRenderer win timeRef models = do
  -- Create a depth buffer object and depth map texture
  (shadowMapTexture, renderShadowMap)
    <- createShadowMapper
  renderScene <- createSceneRenderer shadowMapTexture
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
        Element {
          elementAnimation = Nothing,
          elementModel = getModel models Grass,
          elementPosition = 0,
          elementRotation = Quaternion 1 0
        },
        Element {
          elementAnimation = Just ("spin", 5, worldAnimationTime),
          elementModel = getModel models Pointer,
          elementPosition = worldPointerPosition,
          elementRotation = Quaternion 1 0
        },
        Element {
          elementAnimation = Just ("Spin", 2.08, worldAnimationTime),
          elementModel = getModel models Coin,
          elementPosition = V3 1 0 1,
          elementRotation = Quaternion 1 0
        },
        Element {
          elementAnimation = Just ("Gallop" , 0.67, worldAnimationTime),
          elementModel = getModel models Horse,
          elementPosition = worldPlayerPosition,
          elementRotation = worldPlayerDirection
        }
      ],
      sceneDaylight = Scene.Daylight {
        daylightAmbientIntensity = daylightAmbientIntensity,
        daylightPitch = daylightPitch,
        daylightYaw = daylightYaw
      }
    }
