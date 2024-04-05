module App.Input (
  Input(..)
) where

import Cursor
import Graphics.UI.GLFW

data Input = Input {
  inputCursorPos :: CursorPosition,
  inputMouseButtons :: [(MouseButton, MouseButtonState, ModifierKeys)],
  inputKeys :: [(Key, KeyState, ModifierKeys)],
  inputTime :: Float,
  inputDeltaT :: Float
} deriving (Show)
