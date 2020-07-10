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
        HList a  -> Just $ map' (fromArray a arg) arg
        HString  -> Just $ map' (fromC ht arg) arg
        _        -> let
            f = fromC ht arg
            in case f of
                (Function _ _ _) -> Just $ map' (return' f) arg
                _                -> Nothing
    in case inner of
        Just f  -> Bind (fromC (HList ht) arg)
                        (Lambda [arg] f)
        Nothing -> fromC (HList ht) arg

fromC :: HType -> HAST -> HAST
fromC ht arg = case ht of
    HString  -> f "peekCWString"
    HList _  -> f "peekArray"
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "realToFrac"
    HFloat   -> f "realToFrac"
    _        -> arg
    where f n = Function n [arg] $ fromFFIType ht

convertToC :: HType -> HAST -> HAST
convertToC ht arg = case ht of
    HList a  -> toArray a arg
    _        -> toC ht arg

toArray :: HType -> HAST -> HAST
toArray ht arg = let
    inner = case ht of
        HList a -> Just $ map' (toArray a arg) arg -- (Lambda [arg] $ toC ht arg)) arg
        _       -> case toC ht arg of
            Function _ _ _ -> Just $ map' (toC ht arg) arg
            _              -> Nothing
    in case inner of
        Just f  -> Bind f (Lambda [arg] $ toC (HList ht) arg)
        Nothing -> toC (HList ht) arg

toC :: HType -> HAST -> HAST
toC ht arg = case ht of
    HString  -> f "newCWString"
    HList _  -> f "newArray"
    HTuple _ -> undefined
    HFunc _  -> undefined
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "CDouble"
    HFloat   -> f "CFloat"
    _        -> arg
    where f n  = Function n [arg] $ toFFIType' ht
