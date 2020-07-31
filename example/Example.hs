module Example where

import Foreign.C.Types (CInt)
import Foreign.C.String

someConstant :: Int
someConstant = 63

hello :: IO ()
hello = putStrLn "Hello from Haskell!"

square :: CInt -> CInt
square i = i * i

multisin :: Int -> Double -> Double
multisin x y = (fromIntegral x) * (sin y)

pythasLen :: [Integer] -> Int
pythasLen = length

someComplicatedFunc :: [a] -> IO a
someComplicatedFunc = undefined

mapQuarter :: [Integer] -> [Double]
mapQuarter = map ((*0.25) . fromIntegral)

haskellList :: [Int]
haskellList = [63]

strings :: String -> String
strings = filter (/= 'a')

nested :: [[String]] -> [[String]]
nested = id

tuple :: (Int, String)
tuple = (1, "a")

hsnd :: (Int, Int) -> Int
hsnd (_,b) = b

