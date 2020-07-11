module FFIFinalizer where

import Control.Monad (liftM2, liftM)

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils
import AST

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = case ht of
    HIO t -> f t
    t     -> f t
    where mkFinalizer h = (finalizerName n) ++ " x = " ++ show h
          f t = liftM mkFinalizer $ maybeFinalizerFunc' t

maybeFinalizerFunc' :: HType -> Maybe AST
maybeFinalizerFunc' ht = finalize ht (finalizerVar ht)

finalize :: HType -> AST -> Maybe AST
finalize ht hast = case ht of
      HList a -> freeArray a hast
      _       -> free' ht hast

freeArray :: HType -> AST -> Maybe AST
freeArray ht hast = let
    inner = case ht of
        HList a -> liftM2 map' (freeArray a hast) $ Just hast
        _       -> liftM2 map' (free' ht hast) $ Just hast
    in case inner of
            Just f  -> liftM2 Next (Just $ Bind (fromC (HList ht) hast) $ Lambda [hast] f) $ free' (HList ht) hast
            Nothing -> free' (HList ht) hast

free' :: HType -> AST -> Maybe AST
free' ht hast = case ht of
    HString   -> Just $ f "freeCWString"
    HList  _  -> Just $ f "freeArray"
    HTuple _  -> undefined
    HCPtr  _  -> Just $ f "free"
    _         -> Nothing
    where f n = Function n [hast] $ HIO HUnit

finalizerVar ht = Variable "x" ht
