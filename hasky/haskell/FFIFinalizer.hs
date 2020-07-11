module FFIFinalizer where

import Control.Monad (liftM2, liftM)

import HTypes (HType(..))
import FFIUtils (free', fromC, finalizerName, stripIO)
import AST (AST(..), map')

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = f $ stripIO ht
    where mkFinalizer h = (finalizerName n) ++ ' ':varX:" = " ++ show h
          f = liftM mkFinalizer . maybeFinalizerFunc'

maybeFinalizerFunc' :: HType -> Maybe AST
maybeFinalizerFunc' ht = finalize ht (varXiable [varX]  ht)

varX = 'x'
varA = Variable "a"
varB = Variable "b"
varC = Variable "c"
tuple2 a b = Tuple [varA a , varB b]
tuple3 a b c = Tuple [varA a, varB b, varC c]

finalize :: HType -> AST -> Maybe AST
finalize ht hast = case ht of
    HList a -> freeArray a hast
    HTuple as -> freeTuple as hast
    _       -> free' ht hast

freeArray :: HType -> AST -> Maybe AST
freeArray ht hast = let
    inner  = liftM2 map' (finalize ht hast) $ Just hast
    in case inner of
            Just mp -> liftM2 Next
                       (Just $ Bind (fromC (HList ht) hast) $ Lambda [hast] mp)
                       free
            Nothing -> free
    where free = free' (HList ht) hast

freeTuple :: [HType] -> AST -> Maybe AST
freeTuple as hast = let
    tuple = case as of
        a:b:[]   -> tuple2 a b
        a:b:c:[] -> tuple3 a b c
    inner = case as of
        a:b:[]   -> freeTuple2 (f a varA) $ f b varB
        a:b:c:[] -> freeTuple3 (f a varA) (f b varB) $ f c varC
        _        -> Nothing
        where f = finalize
    in case inner of
        Just inner -> liftM2 Next
                      (Just $ Bind (fromC (HTuple ht) hast) $ Lambda [tuple] inner)
                      free
        Nothing    -> free
    where free = free' (HTuple ht) hast

freeTuple2 :: Maybe AST -> Maybe AST -> Maybe AST
freeTuple2 a b = case (a,b) of
    (Nothing, Nothing) -> Nothing
    (Just fa, Just fb) -> Just $ Next fa fb
    (fa, Nothing) -> fa
    (_ , fb)      -> fb

freeTuple3 :: Maybe AST -> Maybe AST -> Maybe AST -> Maybe AST
freeTuple3 a b c = case (a,b,c) of
    (Nothing, Nothing, Nothing) -> Nothing
    (Just fa, Just fb, Just fc) -> Just $ Next fa $ Next fb fc
    (Just fa, Just fb, Nothing) -> Just $ Next fa fb
    (Just fa, Nothing, Just fc) -> Just $ Next fa fc
    (Nothing, Just fb, Just fc) -> Just $ Next fb fc
    (fa, Nothing, Nothing) -> fa
    (_ , fb, Nothing)      -> fb
    (_ , _ , fc)           -> fb

