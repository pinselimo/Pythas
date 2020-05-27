module HaskyArray (CArray, newArray, peekArray, withArray, free) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

type CArray a = Ptr a