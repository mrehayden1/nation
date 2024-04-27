module App.Input (
  Input(..)
) where

import Graphics.UI.GLFW

import App.Cursor

data Input = Input {
  inputCursorPos :: CursorPosition,
  inputMouseButtons :: [(MouseButton, MouseButtonState, ModifierKeys)],
  inputKeys :: [(Key, KeyState, ModifierKeys)],
  inputTime :: Float,
  inputDeltaT :: Float
} deriving (Show)
