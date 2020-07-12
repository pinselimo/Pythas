module FFIFinalizer where

import Control.Monad (liftM2, liftM)

import HTypes (HType(..))
import FFIUtils (free', fromC, finalizerName, stripIO)
import AST (AST(..), map')

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = f $ stripIO ht
    where mkFinalizer h = (finalizerName n) ++ ' ':var:" = " ++ show h
          f = liftM mkFinalizer . maybeFinalizerFunc'

maybeFinalizerFunc' :: HType -> Maybe AST
maybeFinalizerFunc' ht = finalize ht (Variable [var]  ht)

var = 'x'

finalize :: HType -> AST -> Maybe AST
finalize ht hast = case ht of
      HList a -> freeArray a hast
      _       -> free' ht hast

freeArray :: HType -> AST -> Maybe AST
freeArray ht hast = let
    freeContent = case ht of
                  HList a -> freeArray a hast
                  _       -> free' ht hast
    inner  = liftM2 map' freeContent $ Just hast
    in case inner of
            Just mp -> liftM2 Next
                       (Just $ Bind (fromC (HList ht) hast) $ Lambda [hast] mp)
                       free
            Nothing -> free
    where free = free' (HList ht) hast

