module Test where

import Foreign.C.Types (CInt)
import Foreign.C.String
import HaskyList

constantInt :: Int
constantInt = 63

constantString :: String
constantString = "Hello from Haskell!"

constantList :: [Int]
constantList = [63]

constantTuple :: (Double, String)
constantTuple = (0.63, "Haskell yay")

constantTriple :: (Integer, String, Float)
constantTriple = (63, "63", 0.63)

sideEffects :: IO ()
sideEffects = putStrLn constantString

inputSideEffects :: String -> IO ()
inputSideEffects = putStrLn

pureOperationInt :: Int -> Int
pureOperationInt i = i * i

pureOperationFloat :: Float -> Float -> Float
pureOperationFloat a b = a ** b

pureOperationStrings :: String -> String
pureOperationStrings = filter (/= 'a')

pureOperationMixed :: Int -> Double -> Double
pureOperationMixed x y = (fromIntegral x) * (sin y)

listOfInteger :: [Integer] -> Int
listOfInteger = length

listMixed :: [Integer] -> [Double]
listMixed = map ((*0.25) . fromIntegral)

listNested :: [[[String]]] -> [[[String]]]
listNested ((s:_):_) = [[[head s]]]
listNested [] = [[[""]]]

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

-- TODO: Python error:
{-  File "/home/pinselimo/Python/Hasky/hasky/haskell/parse_type.py", line 70, in hs2py
 -    hs_type_a, hs_type_b = hs_type.split(') (')
 -  ValueError: too many values to unpack (expected 2)
 -}
-- Resulting from FFI Type:
{-
 - CWString -> CWString -> IO (CTuple2  (CArray (CTuple2  (CWString) (CWString))) (CArray (CInt)))
 -}
-- Possible solution: Implement parsing directed at CTuple2
tupleWithListOfTuples :: String -> String -> ([(String, String)],[Int])
tupleWithListOfTuples a b = (take 63 $ repeat (a,b), [63])

noiseFunc :: [a] -> IO a
noiseFunc = undefined

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



