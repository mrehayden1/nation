module App.Env (
  module Control.Monad.Reader,

  App,
  Env(..),
) where

import Control.Monad.Fix
import Control.Monad.Reader
import Reflex

import App.Entity
import App.Input

type App t a = forall m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader (Env t) m) => m a

data Env t = Env {
  envEntities :: Entities,
  envInputE :: Event t Input,
  envTime :: Dynamic t Float,
  envWindowHeight :: Int,
  envWindowWidth :: Int
}
