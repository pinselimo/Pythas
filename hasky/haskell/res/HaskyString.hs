module HaskyString (CWString, newCWString, peekCWString, withCWString, freeCWString, fromCWString) where

import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import System.IO.Unsafe (unsafePerformIO)

fromCWString = unsafePerformIO . peekCWString
freeCWString = free
