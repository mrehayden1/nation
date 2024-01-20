module Util (
  bufferOffset
) where

import Foreign.Ptr (Ptr, nullPtr, plusPtr)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
