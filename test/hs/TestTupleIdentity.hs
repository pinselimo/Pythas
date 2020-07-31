module TestTupleIdentity where

intInt :: (Int, Int) -> (Int, Int)
intInt = id

doubleDouble :: (Double, Double) -> (Double, Double)
doubleDouble = id

intCharDouble :: (Int, Char, Double) -> (Int, Char, Double)
intCharDouble = id

doubleFloatCharDouble :: (Double, Float, Char, Double) -> (Double, Float, Char, Double)
doubleFloatCharDouble = id

-- Lists and Nested

intListStrings :: (Int, [String]) -> (Int, [String])
intListStrings = id

stringListTupleIntChar :: (String, [(Int, Char)]) -> (String, [(Int, Char)])
stringListTupleIntChar = id

nestedIntDouble :: ((Int, Double), (Int, Double)) -> ((Int, Double), (Int, Double))
nestedIntDouble = id

