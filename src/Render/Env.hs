module Render.Env (
  RenderEnv(..),
  aspectRatio
) where

data RenderEnv = RenderEnv {
  viewportHeight :: Int,
  viewportWidth :: Int
}

type AspectRatio = Float

aspectRatio :: RenderEnv -> AspectRatio
aspectRatio RenderEnv{..} = realToFrac viewportWidth / realToFrac viewportHeight
