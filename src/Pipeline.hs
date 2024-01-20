module Pipeline (
  Pipeline (pipelineProgram),

  createPipeline,

  pipelineUniform
) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Graphics.Rendering.OpenGL as GL

import Shaders

data Pipeline = Pipeline {
  pipelineProgram  :: GL.Program,
  -- TODO Make uniforms safer and easier to use
  pipelineUniforms :: Map String GL.UniformLocation
}

-- Partial function, throws error on failure
pipelineUniform :: Pipeline -> String -> GL.UniformLocation
pipelineUniform pipeline name =
  fromMaybe (error errMsg) . Map.lookup name . pipelineUniforms $ pipeline
 where
  errMsg = "Uniform \"" ++ name ++ "\" not found."

createPipeline :: [ShaderLocation] -> IO Pipeline
createPipeline shaders = do
  program <- compileShaders shaders
  uniforms <- GL.activeUniforms program
  uniformLocations <- fmap Map.fromList . forM uniforms $
                        \(_, _, n) -> (n, ) <$> GL.uniformLocation program n
  return $ Pipeline program uniformLocations
