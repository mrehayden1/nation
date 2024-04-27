module App.Render.UI (
  createUiRenderer
) where

import qualified Graphics.Rendering.OpenGL as GL
import Text.Printf

import App.Game
import App.Render.Env
import App.Render.Text

createUiRenderer :: IO (Frame -> Render ())
createUiRenderer = do
  -- TODO Save memory by creating only one text renderer / font
  renderText <- createTextRenderer
  font <- loadFont "bpdots.squares-bold"
  return $ \(_, output)-> do
    -- FIXME duplicated from renderLines
    aspectRatio <- asks viewportAspectRatio
    let screenTop = if aspectRatio > 1
                      then recip aspectRatio
                      else 1
        left = negate . min 1 $ aspectRatio
        top = screenTop - size
        size = 0.04
        coins = worldPlayerCoins . outputWorld $ output
    t <- createText font size (left, top) . printf "Coins: %d" $ coins
    liftIO $ GL.clear [GL.DepthBuffer]
    renderText t False
    deleteText t
