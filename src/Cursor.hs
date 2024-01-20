module Cursor (
  GLFWMouseX,
  GLFWMouseY,
  CursorPosition,

  defaultCursorPositionX,
  defaultCursorPositionY,
  defaultCursorPosition
) where

type GLFWMouseX = Double
type GLFWMouseY = Double

type CursorPosition = (GLFWMouseX, GLFWMouseY)

defaultCursorPositionX :: GLFWMouseX
defaultCursorPositionX = 0

defaultCursorPositionY :: GLFWMouseY
defaultCursorPositionY = 0

defaultCursorPosition :: CursorPosition
defaultCursorPosition = (defaultCursorPositionX, defaultCursorPositionY)
