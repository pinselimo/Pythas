module Example where

import Foreign.C.Types (CInt)

--(HASKY-EXCLUDE someComplicatedFunc

someConstant :: Int
someConstant = 63

hello :: IO ()
hello = putStrLn "Hello from Haskell!"

square :: CInt -> CInt
square i = i * i

foreign export ccall multisin :: Int -> Double -> Double

multisin :: Int -> Double -> Double
multisin x y = (fromIntegral x) * (sin y)

someComplicatedFunc :: String -> IO Int
someComplicatedFunc s@(c:cs) =  return $ length s