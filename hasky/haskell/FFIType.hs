module FFIType where

import FFIUtils
import HTypes (HType(..))

typeDef = " :: "
fec = ("foreign export ccall "++)

makeFFIType :: String -> [HType] -> String
makeFFIType funcname ccompattypes = fec funcname ++ typeDef ++ functype
 where argtypes = typeConcat $ init ccompattypes
       rettype  = ffiType $ last ccompattypes
       typeconcat = foldr (\a b -> ffiType a ++ " -> " ++ b) rettype

createFFIType :: [HType] -> ([HType], [Convert], Convert)
createFFIType ts =
    let fromT = map fromFFIType $ init ts
        toT   = toFFIType (any isIO fromC) $ last ts
        fromC = map fromFFIConvert $ init ts
        toC   = toFFIConvert $ last ts
    in (fromT ++ [toT], fromC, toC)

finalizerExport :: String -> Convert -> HType -> String
finalizerExport n c (HIO t) = needsFinalizer c $ fec 
                            $ finalizerName n 
                            ++ typeDef 
                            ++ ffiType t 
                            ++ " -> IO ()"
finalizerExport _ _ _ = ""

ffiType :: HType -> String
ffiType ht = case ht of
    HUnit   -> "()"
    HCBool  -> "CBool"
    HChar   -> "CChar"
    HWChar  -> "HWchar"
    HSChar  -> "CSChar"
    HUChar  -> "CUChar"
    HShort  -> "CShort"
    HUShort -> "CUShort"
    HCInt   -> "CInt"
    HCUInt  -> "CUInt"
    HLong   -> "CLong"
    HULong  -> "CULong"
    HLLong  -> "CLLong"
    HULLong -> "CULLong"
    HCFloat  -> "CFloat"
    HCDouble -> "CDouble"
    HFloat   -> "Float"
    HDouble  -> "Double"
    HInt    -> "CInt"
    HInteger -> "CLLong"
    HCWString -> "CWString"
    HIO ht'  -> "IO " ++ further ht'
    HCArray ht' -> "CArray " ++ further ht'
    HCList ht'  -> "CList " ++ further ht'
    _ -> fail ("Non C-compatible type \"" ++ show ht ++ "\" in export")
    where further = (\s -> "(" ++ s ++ ")") . ffiType
