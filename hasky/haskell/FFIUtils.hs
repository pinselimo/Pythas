module FFIUtils where

import HTypes (HType(..))

finalizerName = (++"Finalizer")

-- FFI Export Type Construction
toFFIType :: Bool -> HType -> HType
toFFIType anyIO ht = let ht' = toFFIType' ht
        in if anyIO && (not $ isIO ht')
           then HIO ht'
           else ht'

toFFIType' :: HType -> HType
toFFIType' ht = case ht of
 HString -> HIO $ HCWString
 HList x -> HIO $ HCArray $ toFFIType'' x
 HTuple [x] -> undefined
 HFunc [x] -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 HDouble -> HCDouble
 HFloat -> HCFloat
 _ -> ht
 where toFFIType'' ht = stripIO $ toFFIType' ht

fromFFIType :: HType -> HType
fromFFIType ht = case ht of
 HString -> HCWString
 HList x -> HCArray $ fromFFIType x
 HTuple [x] -> undefined
 HFunc [x]  -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 HDouble -> HCDouble
 HFloat -> HCFloat
 _ -> ht

isIO :: HType -> Bool
isIO (HIO _) = True
isIO _ = False

stripIO :: HType -> HType
stripIO ht = case ht of
    HIO ht -> ht
    _      -> ht

