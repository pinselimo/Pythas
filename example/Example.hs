module Example where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

hello :: IO ()
hello = putStrLn "Hello from Haskell!"

square :: CInt -> CInt
square i = i * i

foreign export ccall hssin :: Int -> Double -> Double

hssin :: Int -> Double -> Double
hssin x y = (fromIntegral x) * (sin y)