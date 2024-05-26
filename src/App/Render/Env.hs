module App.Render.Env (
  module Control.Monad.Reader,

  Render,
  runRender,

  Env(..),
  _envPipeline,

  viewportAspectRatio
) where

import Control.Lens
import Control.Monad.Reader
import Data.Function

import App.Render.Model
import App.Render.Pipeline
import App.Render.Scene.Entity

newtype Render a = Render { unRenderer :: ReaderT Env IO a }
 deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runRender :: Render a -> Env -> IO a
runRender r e = flip runReaderT e . unRenderer $ r

data Env = Env {
  envJointModel :: Model,
  envModels :: Models,
  envPipeline :: Pipeline,
  envRenderMeshes :: Bool,
  envShowJoints :: Bool,
  -- Viewport height in pixels
  envViewportHeight :: Int,
  -- Viewport width in pixels
  envViewportWidth :: Int
}

_envModels :: Lens' Env Models
_envModels = lens envModels (\env p -> env { envModels = p })

_envPipeline :: Lens' Env Pipeline
_envPipeline = lens envPipeline (\env p -> env { envPipeline = p })

type AspectRatio = Float

viewportAspectRatio :: Env -> AspectRatio
viewportAspectRatio Env{..} =
  on (/) realToFrac envViewportWidth envViewportHeight
