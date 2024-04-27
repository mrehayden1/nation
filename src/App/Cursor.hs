module App.Cursor (
  GLFWMouseX,
  GLFWMouseY,
  CursorPosition,

  defaultCursorPositionX,
  defaultCursorPositionY,
  defaultCursorPosition
) where

type GLFWMouseX = Float
type GLFWMouseY = Float

type CursorPosition = (GLFWMouseX, GLFWMouseY)

defaultCursorPositionX :: GLFWMouseX
defaultCursorPositionX = 0

defaultCursorPositionY :: GLFWMouseY
defaultCursorPositionY = 0

defaultCursorPosition :: CursorPosition
defaultCursorPosition = (defaultCursorPositionX, defaultCursorPositionY)
