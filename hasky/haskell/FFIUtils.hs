module FFIUtils where

import HTypes (HType(..))
import AST (AST(Function))

finalizerName = (++"Finalizer")

-- FFI Export Type Construction
toFFIType :: Bool -> HType -> HType
toFFIType anyIO ht = let ht' = toFFIType' ht
        in if anyIO && (not $ isIO ht')
           then HIO ht'
           else ht'

toFFIType' :: HType -> HType
toFFIType' ht = case ht of
 HString   -> HIO $ HCWString
 HList x   -> HIO $ HCArray $ toFFIType'' x
 HTuple xs -> HIO $ HTuple $ map toFFIType'' xs
 HFunc xs  -> undefined
 HInteger  -> HLLong
 HInt      -> HCInt
 HBool     -> HCBool
 HDouble   -> HCDouble
 HFloat    -> HCFloat
 _         -> ht
 where toFFIType'' ht = stripIO $ toFFIType' ht

fromFFIType :: HType -> HType
fromFFIType ht = case ht of
 HString    -> HCWString
 HList x    -> HCArray $ fromFFIType x
 HTuple [x] -> undefined
 HFunc [x]  -> undefined
 HInteger   -> HLLong
 HInt       -> HCInt
 HBool      -> HCBool
 HDouble    -> HCDouble
 HFloat     -> HCFloat
 _          -> ht

isIO :: HType -> Bool
isIO (HIO _) = True
isIO _ = False

stripIO :: HType -> HType
stripIO ht = case ht of
    HIO ht -> ht
    _      -> ht

fromC :: HType -> AST -> AST
fromC ht arg = case ht of
    HTuple _ -> f "peek"
    HString  -> f "peekCWString"
    HList _  -> f "peekArray"
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "realToFrac"
    HFloat   -> f "realToFrac"
    _        -> arg
    where f n = Function n [arg] $ fromFFIType ht

toC :: HType -> AST -> AST
toC ht arg = case ht of
    HTuple [a,b,c] -> f "newTuple3"
    HTuple [a,b] -> f "newTuple2"
    HTuple _ -> undefined
    HString  -> f "newCWString"
    HList _  -> f "newArray"
    HFunc _  -> undefined
    HInteger -> f "fromIntegral"
    HInt     -> f "fromIntegral"
    HBool    -> f "fromBool"
    HDouble  -> f "CDouble"
    HFloat   -> f "CFloat"
    _        -> arg
    where f n = Function n [arg] $ toFFIType' ht

free' :: HType -> AST -> Maybe AST
free' ht arg = case ht of
    HString   -> Just $ f "freeCWString"
    HList  _  -> Just $ f "freeArray"
    HTuple _  -> Just $ f "free"
    HCPtr  _  -> Just $ f "free"
    _         -> Nothing
    where f n = Function n [arg] $ HIO HUnit

varA = Variable "a"
varB = Variable "b"
varC = Variable "c"
tuple as = case as of
        a:b:[]   -> Tuple [varA a, varB b]
        a:b:c:[] -> Tuple [varA a, varB b, varC c]

