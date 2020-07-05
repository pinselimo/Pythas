module FFIWrapper where

import HTypes (HType(HIO))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils

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

