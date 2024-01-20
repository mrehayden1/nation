module Shaders (
  ShaderLocation,

  compileShaders
) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GettableStateVar, get, ($=))
import System.FilePath

type ShaderLocation = (FilePath, GL.ShaderType)

{- Compiles a list of shader descriptions into a GL.Program
 - where the shader description ("foo", VertexShader) would cause the shader
 - "./shaders/foo.vert" to be loaded
 -}
compileShaders :: [ShaderLocation] -> IO GL.Program
compileShaders shaders = do
  program <- GL.createProgram
  forM_ shaders . uncurry $ loadShader program
  linkAndCheckProgram program
  return program
 where
  loadShader :: GL.Program -> FilePath -> GL.ShaderType -> IO ()
  loadShader program name shaderType = do
    src <- B.readFile $ "shaders" </> name <> "." <> extension
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader $= src
    compileAndCheck shader
    GL.attachShader program shader
    return ()
   where
    compileAndCheck :: GL.Shader -> IO ()
    compileAndCheck = checked GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

    extension :: String
    extension =
      case shaderType of
        GL.ComputeShader        -> "comp"
        GL.FragmentShader       -> "frag"
        GL.GeometryShader       -> "geom"
        GL.TessControlShader    -> "tesc"
        GL.TessEvaluationShader -> "tese"
        GL.VertexShader         -> "vert"

  linkAndCheckProgram :: GL.Program -> IO ()
  linkAndCheckProgram = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"

  checked :: (t -> IO ())
          -> (t -> GettableStateVar Bool)
          -> (t -> GettableStateVar String)
          -> String
          -> t
          -> IO ()
  checked action getStatus getInfoLog stage object = do
     action object
     ok <- get (getStatus object)
     unless ok $ do
        infoLog <- get (getInfoLog object)
        fail (stage ++ " log: " ++ infoLog)

