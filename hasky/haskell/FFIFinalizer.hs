module FFIFinalizer where

import HTypes (HType(..))
import ParseTypes (TypeDef(funcN, funcT))
import FFIUtils

maybeFinalizerFunc :: HType -> Maybe HAST
maybeFinalizerFunc ht = let finType = toFFIType' ht in
    if needsFinalizer finType
    then Just $ mkFinalizer finType (Variable "x" finType)
    else Nothing

mkFinalizer :: HType -> HAST -> HAST
mkFinalizer ht hast = case ht of
    HCWString   -> Function "freeCWString" [hast] (HIO HUnit)
    HCArray ht' -> Next (map' $ mkFinalizer ht' hast) $ Function "freeArray" [hast] (HIO HUnit)
    HCTuple _   -> undefined
    HCPtr   _   -> Function "free" [hast] (HIO HUnit)
    _           -> id' ht

needsFinalizer :: HType -> Bool
needsFinalizer ht = case ht of
    HCArray _ -> True
    HCWString -> True
    HCTuple _ -> True
    HCPtr _   -> True
    _         -> False
