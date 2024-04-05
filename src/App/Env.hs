module App.Env (
  module Control.Monad.Reader,

  App,
  Env(..),
  Input(..)
) where

import Control.Monad.Reader
import Control.Monad.Fix
import Entity
import Graphics.UI.GLFW
import Reflex

import Cursor

type App t a = forall m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader (Env t) m) => m a

data Env t = Env {
  envEntities :: Entities,
  envInputE :: Event t Input,
  envTime :: Dynamic t Float,
  envWindowHeight :: Int,
  envWindowWidth :: Int
}

data Input = Input {
  inputCursorPos :: CursorPosition,
  inputMouseButtons :: [(MouseButton, MouseButtonState, ModifierKeys)],
  inputKeys :: [(Key, KeyState, ModifierKeys)],
  inputTime :: Float,
  inputDeltaT :: Float
} deriving (Show)
