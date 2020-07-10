module FFIFinalizer where

import Control.Monad (liftM2, liftM)

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils
import FFIWrapper (wrapFunc)

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = case ht of
    HIO t -> f t
    t     -> f t
    where mkFinalizer h = (finalizerName n) ++ " x = " ++ show h
          f t = liftM mkFinalizer $ maybeFinalizerFunc' t

maybeFinalizerFunc' :: HType -> Maybe HAST
maybeFinalizerFunc' ht = let finType = stripIO $ toFFIType' ht in
    finalize finType (Variable "x" finType)

finalize :: HType -> HAST -> Maybe HAST
finalize ht hast = case ht of
      HCArray a -> freeArray a hast
      _         -> free' ht hast

freeArray :: HType -> HAST -> Maybe HAST
freeArray ht hast = let
    inner = case ht of
        HCArray a -> freeArray a hast
        _         -> free' ht hast
    in liftM2 Next (inner >>= (wrap' $ HList ht)) $ free' (HCArray ht) hast

free' :: HType -> HAST -> Maybe HAST
free' ht hast = case ht of
    HCWString   -> Just $ f "freeCWString"
    HCArray _   -> Just $ f "freeArray"
    HCTuple _   -> undefined
    HCPtr   _   -> Just $ f "free"
    _           -> Nothing
    where f n = Function n [hast] $ HIO HUnit

wrap' :: HType -> HAST -> Maybe HAST
wrap' ht hast = case hast of
    Function n _ t -> Just $ wrapFunc n [ht,t] [hast]
    _              -> Nothing
