module FFIUtils where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))

data FromTo = ToC String
            | FromC String
            deriving (Show, Eq)

data Free = Free String
          deriving (Show, Eq)

data Convert = Pure FromTo
             | IOIn FromTo
             | IOOut Free FromTo   -- ↓peek↓
             | Nested Convert Convert String
             deriving (Show, Eq)

data Map = Map
         | MapM

instance Show Map where
  show map = case map of
                Map -> "map"
                MapM -> "mapM"

finalizerName = (++"Finalizer")

putMaps :: Map -> Int -> String
putMaps m i 
 | i > 0 = '(':putMaps' m i ++ ")"
 | otherwise = ""

putMaps' :: Map -> Int -> String
putMaps' mapExp maps
         | maps > 1  = (putMaps mapExp (maps-1)) ++ ' ':'.':' ':show mapExp
         | otherwise = show mapExp

-- FFI Export Type Construction
toFFIType :: Bool -> HType -> HType
toFFIType anyIO ht = let ht' = toFFIType' ht
        in case ht' of
             HIO _ -> ht'
             _     -> if anyIO then HIO ht' else ht'

toFFIType' :: HType -> HType
toFFIType' ht = case ht of
 HString -> HIO HCWString
 HList x -> HIO $ HCArray $ toFFIType'' x
 HTuple [x] -> undefined
 HFunc [x] -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 HDouble -> HCDouble
 HFloat -> HCFloat
 _ -> ht
 where toFFIType'' ht = let ht' = toFFIType' ht
                        in case ht' of
                          HIO ht'' -> ht''
                          _        -> ht'

fromFFIType :: HType -> HType
fromFFIType ht = case ht of
 HString -> HCWString
 HList x -> HCArray $ fromFFIType x
 HTuple [x] -> undefined
 HFunc [x]  -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 _ -> ht

-- FFI Export Type Converter Construction
toFFIConvert :: HType -> Convert
toFFIConvert ht = case ht of
 HString -> IOOut (Free "freeCWString") $ ToC "newCWString"
 HList x -> Nested (IOOut (Free "freeArray") $ ToC "newArray") (toFFIConvert x) "peekArray"
 HTuple [x] -> undefined -- TODO Tuples
 HFunc  [x] -> undefined -- TODO Functions
 HInteger -> Pure $ ToC "fromIntegral"
 HInt -> Pure $ ToC "fromIntegral"
 HBool -> Pure $ ToC "fromBool"
 HDouble -> Pure $ ToC "CDouble"
 HFloat -> Pure $ ToC "CFloat"
 _ -> Pure $ ToC "id"

fromFFIConvert :: HType -> Convert
fromFFIConvert ht = case ht of
 HString -> IOIn $ FromC "peekCWString"
 HList x -> Nested (IOIn $ FromC "peekArray") (fromFFIConvert x) "peekArray"
 HTuple [x] -> undefined -- TODO Tuples
 HFunc  [x] -> undefined -- TODO Functions
 HInteger -> Pure $ FromC "fromIntegral"
 HInt -> Pure $ FromC "fromIntegral"
 HBool -> Pure $ FromC "fromBool"
 _ -> Pure $ FromC "id"

isIO :: Convert -> Bool
isIO (Pure _) = False
isIO (Nested a b _) = isIO a || isIO b
isIO _ = True

needsFinalizer :: Convert -> Bool
needsFinalizer (IOOut _ _) = True
needsFinalizer (Nested a b _) = True
needsFinalizer _ = False

-- Writer functions
sp s = ' ':s
tab = "\n    "
bindr = " =<< "
bind = " >>= "
cash = " $ "
ring = " . "
equals = " = "
concat' :: [Char] -> String
concat' = foldr (\a b->' ':a:b) ""
concatNL :: [String] -> String
concatNL = foldr (\a b -> a ++ tab ++ b) ""
parens s = '(':s++")"
return' = "return"
