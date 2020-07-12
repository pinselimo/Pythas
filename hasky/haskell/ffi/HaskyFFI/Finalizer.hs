module HaskyFFI.Finalizer where

import Control.Monad (liftM2, liftM)

import HaskyFFI.HTypes (HType(..), stripIO)
import HaskyFFI.AST (AST(..), map')
import HaskyFFI.Utils (free', fromC, finalizerName, tuple, varA, varB, varC)

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = f $ stripIO ht
    where mkFinalizer h = (finalizerName n) ++ ' ':varX:" = " ++ show h
          f = liftM mkFinalizer . maybeFinalizerFunc'

maybeFinalizerFunc' :: HType -> Maybe AST
maybeFinalizerFunc' ht = finalize ht (Variable [varX]  ht)

varX = 'x'

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
    inner = case as of
        a:b:[]   -> freeTuple2 (f a varA) (f b varB)
        a:b:c:[] -> freeTuple3 (f a varA) (f b varB) $ f c varC
        _        -> Nothing
        where f t v = finalize t $ v t
    in case inner of
        Just inner -> liftM2 Next
                      (Just $ Bind (fromC (HTuple as) hast) $ Lambda [tuple as] inner)
                      free
        Nothing    -> free
    where free = free' (HTuple as) hast

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
    (_ , _ , fc)           -> fc

