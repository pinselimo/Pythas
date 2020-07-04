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

type Body = [HAST]
type Args = [HAST]

data HAST = Function String Args Body HType
          | Variable String HType
          deriving (Show, Eq)

getHASTType :: HAST -> HType
getHASTType h = case h of
    Function _ _ _ t -> t
    Variable _ t     -> t

return' :: HAST -> HAST
return' h = Function "return" [] [h] $ HIO $ getHASTType h

showHAST :: HAST -> String
showHAST h = case h of
    (Variable n _) -> ' ':n
    _ -> case getHASTType h of
            (HIO _)  -> showFunc h
            _        -> showFunc h

showIOFunc :: HAST -> String
showIOFunc (Function n args body ht) = ' ':parens (before ++ n ++ end)
    where end = concat (map showHAST args')
          before = concatNL $ zipWith (\f a -> f ++ bind ++ '\\':a++" -> ") iofncs ioargs
          iofncs = map showIOFunc $ getIOs body
          ioargs = map showIOFunc $ getIOs args
          getIOs = filter (\h -> isIO $ getHASTType h)
          args'  = zipWith (\a b -> if isIO $ getHASTType b then a else b) args body

showFunc :: HAST -> String
showFunc (Function n args body ht) = ' ':parens (n ++ body')
    where body' = concat $ map showHAST body

wrap :: TypeDef -> HAST
wrap td = toC $ wrapArgs (funcN td) (last ts) args fromC
    where ts = funcT td
          toC = convertToC (last ts)
          fromC = zipWith convertFromC (init ts) args
          args  = zipWith (\c t -> Variable [c] t) ['a'..'z'] (map fromFFIType $ init ts)

wrapArgs :: String -> HType -> [HAST] -> [HAST] -> HAST
wrapArgs fn ht args convs
    | any isIO htin && not (isIO ht) = return' norm
    | otherwise                      = norm
    where norm = Function fn args convs ht
          htin = map getHASTType convs

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

convertFromC :: HType -> HAST -> HAST
convertFromC ht arg =
    let args = [arg]
    in case ht of
    HString -> Function "peekCWString"  [] args (HIO HString)
    HList a -> map' $ convertFromC a (Function "peekArray" args args (HIO (HList a)))
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> Function "fromIntegral" [] args ht
    HInt     -> Function "fromIntegral" [] args ht
    HBool    -> Function "fromBool"     [] args ht
    HDouble  -> Function "realToFrac"   [] args ht
    HFloat   -> Function "realToFrac"   [] args ht
    _        -> arg

convertToC :: HType -> HAST -> HAST
convertToC ht arg = let
        ht'  = toFFIType' ht
        args = [arg]
    in case ht of
    HString  -> Function "newCWString"  [] args ht'
    HList a  -> Function "newArray"     [] [map' (convertToC a arg)] ht'
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> Function "fromIntegral" [] args ht'
    HInt     -> Function "fromIntegral" [] args ht'
    HBool    -> Function "fromBool"     [] args ht'
    HDouble  -> Function "CDouble"      [] args ht'
    HFloat   -> Function "CFloat"       [] args ht'
    _        -> arg

map' :: HAST -> HAST
map' a = case getHASTType a of
    (HIO ht) -> Function "mapM" [a] [a] (HIO (HList ht))
    ht       -> Function "map" [a] [a] (HList ht)

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
