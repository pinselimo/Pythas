module FFIWrapper where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils

wrap :: String -> [HType] -> HAST
wrap fn fts
    | (isIO $ getHASTType func) && (isIO $ toFFIType' ft) = Bind func (Lambda [res] $ toC res)
    | isIO  $ getHASTType func = Bind func (Lambda [res] $ return' $ toC res)
    | otherwise               = toC  func
    where func = wrapArgs fn fts args
          ft   = last fts
          ats  = init fts
          res = Variable "res" ft
          toC = convertToC ft
          args  = zipWith (\c t -> Variable [c] t) ['a'..'z'] ats

wrapArgs :: String -> [HType] -> [HAST] -> HAST
wrapArgs fn fts args = foldr ($) (mkFunc fn fts) convfuncs
    where convfuncs = zipWith convertFromC fts args

mkFunc :: String -> [HType] -> HAST
mkFunc fn hts
    | any isIO htin && not (isIO $ last hts) = return' norm
    | otherwise                      = norm
    where norm = Function fn [] $ last hts
          htin = map fromFFIType $ init hts

convertFromC :: HType -> HAST -> HAST -> HAST
convertFromC ht arg f = let
    in case ht of
    HString -> Bind (fromC ht arg)
                    (Lambda [arg] $ adf arg)
    HList a -> Bind (fromArray a arg)
                    (Lambda [arg] $ adf arg)

    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    _          -> adf $ fromC ht arg
    where adf = add f

fromArray :: HType -> HAST -> HAST
fromArray ht arg = let
    inner = case ht of
        HList a  -> map' (fromArray a arg) arg
        HString  -> map' (fromC ht arg) arg
        _        -> map' (return' $ fromC ht arg) arg
    in Bind (fromC (HList ht) arg)
            (Lambda [arg] inner)

fromC :: HType -> HAST -> HAST
fromC ht arg = let
        ht' = fromFFIType ht
    in case ht of
    HString  -> Function "peekCWString" [arg] ht'
    HList _  -> Function "peekArray"    [arg] ht'
    HInteger -> Function "fromIntegral" [arg] ht'
    HInt     -> Function "fromIntegral" [arg] ht'
    HBool    -> Function "fromBool"     [arg] ht'
    HDouble  -> Function "realToFrac"   [arg] ht'
    HFloat   -> Function "realToFrac"   [arg] ht'
    _        -> arg

convertToC :: HType -> HAST -> HAST
convertToC ht arg = let
        ht'  = toFFIType' ht
        f n  = Function n [arg] ht'
    in case ht of
    HString  -> f "newCWString"
    HList a  -> let
        inner = map' (convertToC a arg) arg
        in if isIO $ getHASTType inner
           then Bindl (Function "newArray" [] ht') inner
           else Function "newArray" [inner] ht'
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "CDouble"
    HFloat   -> f "CFloat"
    _        -> arg

