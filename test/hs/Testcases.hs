module Testcases where

import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr (castPtr)
import Foreign.Pythas.Tuples (CTuple2, newTuple2)

constantInt :: Int
constantInt = 63

constantString :: String
constantString = "Hello from Haskell!"

constantList :: [Int]
constantList = [63]

constantTuple :: (Double, String)
constantTuple = (0.63, "Haskell yay")

testAlignment :: [(Double, CInt, CChar)]
testAlignment = take 10 $ repeat (0.1, 63, castCharToCChar 'a')

constantTriple :: (Int, Double, Float)
constantTriple = (63, 0.63, 0.63)

constantQuadruple :: (Int, Double, CChar, CChar)
constantQuadruple = (63, 0.1, 63, castCharToCChar 'a')

sideEffects :: IO ()
sideEffects = putStrLn constantString

inputSideEffects :: String -> IO ()
inputSideEffects = putStrLn

pureOperationInt :: Int -> Int
pureOperationInt i = i * i

pureOperationFloat :: Float -> Float -> Float
pureOperationFloat a b = a ** b

pureOperationString :: String -> String
pureOperationString = filter (/= 'a')

pureOperationMixed :: Int -> Double -> Double
pureOperationMixed x y = (fromIntegral x) * (sin y)

listOfInteger :: [Integer] -> Int
listOfInteger = length

listMixed :: [Integer] -> [Double]
listMixed = map ((*0.25) . fromIntegral)

listNested :: [[[String]]] -> String
listNested ((s:_):_) = case s of
                        (x:_) -> x
                        _     -> ""
listNested _ = ""

listOfTuples :: String -> CInt-> [(String, CInt)]
listOfTuples a b = [(a,b)]

listOfTuplesNested :: String -> Integer -> [[(Integer, String)]]
listOfTuplesNested a b = [[(b,a)]]

listOfTuplesWithList :: [String] -> [[([[String]], [Integer])]]
listOfTuplesWithList xs = [[(take 63 $ repeat xs, [63])]]

tupleWithList :: Int -> ([String], [Int])
tupleWithList l = (take l $ repeat "Haskell", take l $ repeat 63)

tupleWithNestedList :: Integer -> String -> ([[String]],[[Integer]])
tupleWithNestedList i s = ([take (fromIntegral i) $ repeat s], [[i]])

tupleWithListOfTuples :: String -> String -> ([(String, String)],[Int])
tupleWithListOfTuples a b = (take 63 $ repeat (a,b), [63])

noiseFunc :: [a] -> IO a
noiseFunc = undefined

type CustomType = CTuple2 CInt CInt

takesCustomType :: CustomType -> CTuple2 CInt CInt
takesCustomType = id

makesCustomType :: CInt -> CInt -> IO CustomType
makesCustomType a b = newTuple2 (a, b)

foreign export ccall takesCustomType :: CustomType -> CTuple2 CInt CInt
foreign export ccall makesCustomType :: CInt -> CInt -> IO CustomType

type NoiseType = String
newtype NoiseNewType = NNT {
    fromNoise :: String
    } deriving (Show, Eq)
data NoiseSumType = Foo
                  | Bar
                  deriving (Eq, Show)
data NoiseRecord a b = NR
        { getA :: a
        , getB :: b
        } deriving (Eq)
class NoiseClass a where
    noise :: a -> String
instance NoiseClass (NoiseRecord a b) where
    noise _ = "MoreNoise"
-- Whitespace left intentionally to check parser
--



