module FFIUtils where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT, TypeDef))

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

data HAST = Function String [HAST] HType
          | Variable String HType
          | Bind     HAST HAST
          | Lambda   [HAST] HAST
          deriving (Show, Eq)

getHASTType :: HAST -> HType
getHASTType h = case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> getHASTType b
    Lambda as b    -> getHASTType b

return' :: HAST -> HAST
return' (Function n args ht) = Function ("return $ " ++ n) args $ HIO ht

showHAST :: HAST -> String
showHAST h = case h of
    (Variable n _) -> ' ':n
    (Lambda as bd) -> ' ':parens ("\\" ++ (concat $ map showHAST as) ++ " ->\n   " ++ showHAST bd)
    (Bind a b)     -> showHAST a ++ " >>=" ++ showHAST b
    (Function n as _) -> ' ':parens (n ++ (concat $ map showHAST as))

wrap :: TypeDef -> HAST
wrap td = wrapArgs (funcN td) ts args
    where ts = funcT td
          toC = convertToC (last ts)
          args  = zipWith (\c t -> Variable [c] t) ['a'..'z'] (init ts)

wrapArgs :: String -> [HType] -> [HAST] -> HAST
wrapArgs fn hts args = foldr ($) (mkFunc fn hts) convfuncs
    where convfuncs = zipWith convertFromC hts args

mkFunc :: String -> [HType] -> HAST
mkFunc fn hts
    | any isIO htin && not (isIO $ last hts) = return' norm
    | otherwise                      = norm
    where norm = Function fn [] $ last hts
          htin = map fromFFIType hts

finalize :: TypeDef -> HAST
finalize = undefined

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
 HDouble -> HCDouble
 HFloat -> HCFloat
 _ -> ht

convertFromC :: HType -> HAST -> HAST -> HAST
convertFromC ht arg f =
    case ht of
    HString -> Bind (Function "peekCWString"  [arg] (HIO HString))
                    (Lambda [arg] $ adf arg)
    HList a -> Bind (Function "peekArray" [arg] (HIO (HList a)))
                    (Lambda [arg] (map' (convertFromC a arg f) f))
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> adf $ Function "fromIntegral" [arg] ht
    HInt     -> adf $ Function "fromIntegral" [arg] ht
    HBool    -> adf $ Function "fromBool"     [arg] ht
    HDouble  -> adf $ Function "realToFrac"   [arg] ht
    HFloat   -> adf $ Function "realToFrac"   [arg] ht
    _        -> adf arg
    where adf = add f

convertToC :: HType -> HAST -> HAST
convertToC ht arg = let
        ht'  = toFFIType' ht
        f n  = Function n [arg] ht'
    in case ht of
    HString  -> f "newCWString"
    HList a  -> let m = map' (convertToC a arg) arg in
                case getHASTType m of
                (HIO _) -> Bind m (Lambda [arg] $ f "newArray")
                _       -> Function "newArray" [m] ht'
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "CDouble"
    HFloat   -> f "CFloat"
    _        -> arg

add :: HAST -> HAST -> HAST
add hast hast' = case hast of
    (Function fn args ft) -> Function fn (hast':args) ft
    (Bind a b)            -> Bind a $ add b hast'
    (Lambda as b)         -> Lambda as $ add b hast'

map' :: HAST -> HAST -> HAST
map' a f = case getHASTType a of
    (HIO ht) -> Bind (Function "mapM" [a] (HIO (HList ht)))
                     (Lambda [a] $ add f a)
    ht       -> add f $ Function "map" [a] (HList ht)

isIO :: HType -> Bool
isIO (HIO _) = True
isIO _ = False

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
