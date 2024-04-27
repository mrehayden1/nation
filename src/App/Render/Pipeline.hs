-- TODO Move this all into the render environment?
module App.Render.Pipeline (
  Pipeline,

  compilePipeline,

  bindPipeline,
  unbindPipeline,

  pipelineUniform,
  pipelineUniformV,
  pipelineUniformMatrix4v
) where

import Control.Monad
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.StateVar
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GL
import Linear
import Text.Printf

import App.Render.Shaders

data Pipeline = Pipeline {
  pipelineProgram  :: GL.Program,
  -- TODO Make uniforms easier to use (and maybe typesafe?)
  pipelineUniforms :: Map String GL.UniformLocation
}

pipelineUniform :: (GL.Uniform a) => Pipeline -> String -> StateVar a
pipelineUniform pipeline name = do
  let location = Map.lookup name . pipelineUniforms $ pipeline
  maybe noOpStateVar GL.uniform location
 where
  noOpStateVar :: StateVar a
  noOpStateVar = makeStateVar undefined $ \_ -> do
    printf "pipelineUniform: warning: uniform '%s' not found.\n" name

pipelineUniformV :: forall a. (Storable a, GL.Uniform a)
  => Pipeline
  -> String
  -> Vector a
  -> IO ()
pipelineUniformV pipeline name vec = do
  let location = Map.lookup (name ++ "[0]") . pipelineUniforms $ pipeline
      size = fromIntegral $ sizeOf (undefined :: a) * SV.length vec
  case location of
    Nothing -> printf "pipelineUniform: warning: uniform '%s' not found.\n" name
    Just l -> SV.unsafeWith vec (GL.uniformv l size)

pipelineUniformMatrix4v :: Foldable f
  => Pipeline
  -> String
  -> f (M44 Float)
  -> IO ()
pipelineUniformMatrix4v pipeline name mats = do
  let vec = SV.fromList . concatMap (concatMap toList . toList) $ mats
      location = Map.lookup (name ++ "[0]") . pipelineUniforms $ pipeline
      n = fromIntegral . length $ mats
  case location of
    Nothing -> printf "pipelineUniform: warning: uniform '%s' not found.\n" name
    Just (GL.UniformLocation l) -> do
      -- M44s are stored in row major order so set transpose flag to true
      SV.unsafeWith vec (glUniformMatrix4fv l n 1)

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
