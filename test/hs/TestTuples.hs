module TestTuples where

import Foreign.C.Types
import Foreign.C.String (castCharToCChar)

-- 2 fields

int32Double :: CInt -> Double ->  (CInt, Double)
int32Double = (,)

doubleInt32 :: Double -> CInt -> (Double, CInt)
doubleInt32 = (,)

charInt32 :: CChar -> CInt -> (CChar, CInt)
charInt32 = (,)

int32Char :: CInt -> CChar -> (CInt, CChar)
int32Char = (,)

-- Nested

list2A :: [CInt] -> String -> ([CInt], String)
list2A = (,)

list2B :: CInt -> [String] -> (CInt, [String])
list2B = (,)

-- 3 fields

int32DoubleChar :: CInt -> Double -> CChar ->  (CInt, Double, CChar)
int32DoubleChar = (,,)

doubleInt32Char :: Double -> CInt -> CChar -> (Double, CInt, CChar)
doubleInt32Char = (,,)

charInt32Double :: CChar -> CInt -> Double -> (CChar, CInt, Double)
charInt32Double = (,,)

int32CharChar :: CInt -> CChar -> CChar -> (CInt, CChar, CChar)
int32CharChar = (,,)

-- Nested

list3A :: [CInt] -> String -> CFloat -> ([CInt], String, CFloat)
list3A = (,,)

list3B :: CInt -> [String] -> CFloat -> (CInt, [String], CFloat)
list3B = (,,)

list3C :: CInt -> String -> [CFloat] -> (CInt, String, [CFloat])
list3C = (,,)

-- 4 fields

int32DoubleDoubleChar :: CInt -> Double -> Double -> CChar -> (CInt, Double, Double, CChar)
int32DoubleDoubleChar = (,,,)

doubleInt32CharChar :: Double -> CInt -> CChar -> CChar -> (Double, CInt, CChar, CChar)
doubleInt32CharChar = (,,,)

charInt32DoubleChar :: CChar -> CInt -> Double -> CChar -> (CChar, CInt, Double, CChar)
charInt32DoubleChar = (,,,)

int32CharCharChar :: CInt -> CChar -> CChar -> CChar -> (CInt, CChar, CChar, CChar)
int32CharCharChar = (,,,)

-- Nested

list4A :: [CInt] -> String -> CFloat -> Double -> ([CInt], String, CFloat, Double)
list4A = (,,,)

list4B :: CInt -> [String] -> CFloat -> Double -> (CInt, [String], CFloat, Double)
list4B = (,,,)

list4C :: CInt -> String -> [CFloat] -> Double -> (CInt, String, [CFloat], Double)
list4C = (,,,)

list4D :: CInt -> String -> CFloat -> [CDouble] -> (CInt, String, CFloat, [CDouble])
list4D = (,,,)
