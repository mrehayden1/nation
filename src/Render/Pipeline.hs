module Render.Pipeline (
  Pipeline,

  compilePipeline,

  bindPipeline,
  unbindPipeline,

  pipelineUniform
) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL

import Render.Shaders

data Pipeline = Pipeline {
  pipelineProgram  :: GL.Program,
  -- TODO Make uniforms easier to use (and maybe typesafe?)
  pipelineUniforms :: Map String GL.UniformLocation
}

pipelineUniform :: (GL.Uniform a) => Pipeline -> String -> StateVar a
pipelineUniform pipeline name = do
  let location = Map.lookup name . pipelineUniforms
                   $ pipeline
  maybe noOpStateVar GL.uniform location
 where
  noOpStateVar :: StateVar a
  noOpStateVar = makeStateVar undefined $ \_ -> return ()

compilePipeline :: [ShaderLocation] -> IO Pipeline
compilePipeline shaders = do
  program <- compileShaders shaders
  uniforms <- GL.activeUniforms program
  uniformLocations <- fmap Map.fromList . forM uniforms $
                        \(_, _, n) -> (n, ) <$> GL.uniformLocation program n
  return $ Pipeline program uniformLocations

-- TODO Wrap this statevar for a more explicitly mutable API
bindPipeline :: Pipeline -> IO ()
bindPipeline pipeline =
  GL.currentProgram $= (Just . pipelineProgram $ pipeline)

unbindPipeline :: IO ()
unbindPipeline = GL.currentProgram $= Nothing
