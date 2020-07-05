module FFIWrapper where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils

wrap :: String -> [HType] -> HAST
wrap fn fts
    | (isIO $ getHASTType func) && (isIO $ toFFIType' ft) = Bind func (Lambda [res] $ toC res)
    | isIO $ getHASTType func = Bind func (Lambda [res] $ return' $ toC res)
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
        ht' = fromFFIType ht
    in case ht of
    HString -> Bind (convertFromC' ht ht' arg)
                    (Lambda [arg] $ adf arg)
    HList a -> Bind (Function "peekArray" [arg] ht')
                    (Lambda [arg] body)
                where m = map' (convertFromC' a (fromFFIType a) arg) arg
                      body = if isIO $ getHASTType m
                           then Bind m (Lambda [arg] $ adf arg)
                           else adf m
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    _          -> adf $ convertFromC' ht ht' arg
    where adf = add f

convertFromC' :: HType -> HType -> HAST -> HAST
convertFromC' ht ht' arg = case ht of
    HString  -> Function "peekCWString" [arg] ht'
    HList a  -> Bind (Function "peekArray" [arg] ht')
                     (Lambda [arg] body)
                where m = map' (convertFromC' a (fromFFIType a) arg) arg
                      body = if isIO $ getHASTType m
                           then m
                           else return' m
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
    HList a  -> let m = map' (convertToC a arg) arg in
                case getHASTType m of
                (HIO _) -> Bind m (Lambda [Variable "res" a] $ convertToC a $ Variable "res" a)
                _       -> Function "newArray" [m] ht'
    HTuple [a] -> undefined
    HFunc  [a] -> undefined
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "CDouble"
    HFloat   -> f "CFloat"
    _        -> arg

