module Main (
  main
) where

import Control.Monad
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import System.Exit

import Shaders

name :: String
name = "Nation"

main :: IO ()
main = do
  win <- openWindow
  -- Load shaders
  program <- loadShaders
  GL.currentProgram $= Just program
  -- Create & load vertices
  let vertices = fmap (uncurry GL.Vertex2) $
        [(-0.5, -0.5), (0.5, -0.5), (0, 0.5)] :: [GL.Vertex2 Float]
      numVertices = length vertices
      firstIndex = 0
      vPosition = GL.AttribLocation 0
  triangles <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triangles
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  GL.vertexAttribPointer vPosition $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (bufferOffset firstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled
  -- Loop and exit
  _ <- forever $ do
         _ <- GLFW.pollEvents
         GL.clearColor $= GL.Color4 0 0 0 1
         GL.clear [GL.ColorBuffer]
         GL.bindVertexArrayObject $= Just triangles
         GL.drawArrays GL.Triangles firstIndex . fromIntegral $ numVertices
         GLFW.swapBuffers win
  GLFW.destroyWindow win
  GLFW.terminate
 where
  openWindow :: IO GLFW.Window
  openWindow = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    win <- fmap (maybe (error "GLFW failed to create window.") id)
             . GLFW.createWindow 600 600 name Nothing $ Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

  shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
