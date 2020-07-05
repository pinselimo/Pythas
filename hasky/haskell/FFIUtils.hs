module FFIUtils where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT, TypeDef))

data HAST = Function String [HAST] HType
          | Variable String HType
          | Bind     HAST HAST
          | Lambda   [HAST] HAST
          deriving (Show, Eq)

getHASTType :: HAST -> HType
getHASTType h = case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> HIO $ getHASTType b
    Lambda as b    -> getHASTType b

-- TODO implement this proper
return' :: HAST -> HAST
return' (Function n args ht) = Function ("return $ " ++ n) args $ HIO ht
return' (Variable n ht)      = Variable ("return $ " ++ n) $ HIO ht

showHAST :: HAST -> String
showHAST h = case h of
    (Variable n _) -> ' ':n
    (Lambda as bd) -> ' ':parens ("\\" ++ (concat $ map showHAST as) ++ " ->\n   " ++ showHAST bd)
    (Bind a b)     -> showHAST a ++ " >>=" ++ showHAST b
    (Function n as _) -> ' ':parens (n ++ (concat $ map showHAST as))

wrap :: TypeDef -> HAST
wrap td
    | (isIO $ getHASTType func) && (isIO $ toFFIType' ft) = Bind func (Lambda [res] $ toC res)
    | isIO $ getHASTType func = Bind func (Lambda [res] $ return' $ toC res)
    | otherwise               = toC  func
    where func = wrapArgs td args
          ft   = last $ funcT td
          ts   = init $ funcT td
          res = Variable "res" ft
          toC = convertToC ft
          args  = zipWith (\c t -> Variable [c] t) ['a'..'z'] ts

wrapArgs :: TypeDef -> [HAST] -> HAST
wrapArgs td args = foldr ($) (mkFunc (funcN td) hts) convfuncs
    where convfuncs = zipWith convertFromC hts args
          hts = funcT td

mkFunc :: String -> [HType] -> HAST
mkFunc fn hts
    | any isIO htin && not (isIO $ last hts) = return' norm
    | otherwise                      = norm
    where norm = Function fn [] $ last hts
          htin = map fromFFIType hts

finalize :: TypeDef -> HAST
finalize = undefined

finalizerName = (++"Finalizer")

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
convertFromC ht arg f = let
        ht' = fromFFIType ht
    in case ht of
    HString -> Bind (Function "peekCWString"  [arg] (HIO ht'))
                    (Lambda [arg] $ adf arg)
    HList a -> Bind (Function "peekArray" [arg] (HIO ht'))
                    (Lambda [arg] (map' (convertFromC a arg f)))
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> adf $ Function "fromIntegral" [arg] ht'
    HInt     -> adf $ Function "fromIntegral" [arg] ht'
    HBool    -> adf $ Function "fromBool"     [arg] ht'
    HDouble  -> adf $ Function "realToFrac"   [arg] ht'
    HFloat   -> adf $ Function "realToFrac"   [arg] ht'
    _        -> adf arg
    where adf = add f

convertToC :: HType -> HAST -> HAST
convertToC ht arg = let
        ht'  = toFFIType' ht
        f n  = Function n [arg] ht'
    in case ht of
    HString  -> f "newCWString"
    HList a  -> let m = map' (convertToC a arg) in
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

id' :: HType -> HAST
id' = Function "id" []

add :: HAST -> HAST -> HAST
add hast hast' = case hast of
    (Function fn args ft) -> Function fn (hast':args) ft
    (Bind a b)            -> Bind a $ add b hast'
    (Lambda as b)         -> Lambda as $ add b hast'

map' :: HAST -> HAST
map' a = case getHASTType a of
    (HIO ht) -> Function "mapM" [a] (HIO (HList ht))
    _        -> case a of
        (Function n (a':as) ht) -> let -- remove last add and map over it
            (Function conv (x:_) t) = a'
            in add (Function n as ht) $ Function "map" [Function conv [] t,x] (HList ht)
        _ -> a

isIO :: HType -> Bool
isIO (HIO _) = True
isIO _ = False

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
