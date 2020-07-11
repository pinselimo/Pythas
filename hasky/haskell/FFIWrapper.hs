module FFIWrapper where

import HTypes (HType(..))
import AST
import FFIUtils

wrap :: String -> String -> [HType] -> String
wrap modname funcname functype = funcname ++ (concat $ map show args) ++ " = " ++ show body
    where body = wrapFunc (modname ++ '.':funcname) functype args
          args = zipWith (\c t -> Variable [c] t) ['a'..'z'] $ init functype

wrapFunc :: String -> [HType] -> [AST] -> AST
wrapFunc fn fts args = wrapAST func ft
    where func = wrapArgs fn fts args
          ft   = last fts

wrapAST :: AST -> HType -> AST
wrapAST func ft
    | (isIO $ typeOf func) && (isIO $ toFFIType' ft) = case ft of
                   HIO HUnit -> func
                   _         -> Bind func (Lambda [res] $ toC res)
    | isIO $ typeOf func = Bind func (Lambda [res] $ return' $ toC res)
    | otherwise               = case ft of
         (HList _) -> Bind (return' func) (Lambda [res] $ toC res)
         _         -> toC func
    where res  = Variable "res" ft
          toC  = convertToC ft

wrapArgs :: String -> [HType] -> [AST] -> AST
wrapArgs fn ts args = foldr ($) (mkFunc fn ts) convfuncs
    where convfuncs = zipWith convertFromC ts args

mkFunc :: String -> [HType] -> AST
mkFunc fn ts = let
    htin = map toFFIType' $ init ts
    in if any isIO htin && not (isIO $ last ts)
    then return' norm
    else norm
    where norm = Function fn []  $ last ts

convertFromC :: HType -> AST -> AST -> AST
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

fromArray :: HType -> AST -> AST
fromArray ht arg = let
    inner = case ht of
        HList a  -> Just $ map' (fromArray a arg) arg
        HString  -> Just $ map' (fromC ht arg) arg
        _        -> let
            f = fromC ht arg
            in case f of
                Function _ _ (HIO _) -> Just $ map' f arg
                Function _ _ _       -> Just $ map' (return' f) arg
                _                -> Nothing
    in case inner of
        Just f  -> Bind (fromC (HList ht) arg)
                        (Lambda [arg] f)
        Nothing -> fromC (HList ht) arg

fromC :: HType -> AST -> AST
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

convertToC :: HType -> AST -> AST
convertToC ht arg = case ht of
    HList a  -> toArray a arg
    _        -> toC ht arg

toArray :: HType -> AST -> AST
toArray ht arg = let
    inner = case ht of
        HList a -> Just $ map' (toArray a arg) arg
        _       -> case toC ht arg of
            Function _ _ _ -> Just $ map' (toC ht arg) arg
            _              -> Nothing
    in case inner of
        Just f  -> Bind (return' f) (Lambda [arg] $ toC (HList ht) arg)
        Nothing -> toC (HList ht) arg

toC :: HType -> AST -> AST
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
