module FFICreate (createFFI) where

import ParseTypes (TypeDef(funcN, funcT))
import FFIType
import FFIWrapper
import FFIUtils (needsFinalizer)

imports = map ("import "++)
          ["Foreign.C.Types"
          ,"Foreign.Marshal.Utils (fromBool, toBool)"
          ,"Foreign.Marshal.Alloc (free)"
          ,"HaskyList"
          ,"HaskyArray"
          ,"HaskyTuple"
          ,"HaskyString"]

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> (FilePath, String)
createFFI fn modname exports typeDefs =
 let ffiFilename = takeWhile (/='.') fn ++ "_hasky_ffi.hs"
     ffiModname = modname ++ "_hasky_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = map (makeFFIExport modname) exportedFuncTypes
     ffiContent = "{-# LANGUAGE ForeignFunctionInterface #-}\n"
             ++ "module " ++ ffiModname
             ++ " where\n\n"
             ++ "import qualified " ++ modname ++ "\n\n"
             ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

 in (ffiFilename, ffiContent)

makeFFIExport :: String -> TypeDef -> String
makeFFIExport modname typedef = let
     (functype, fromC, toC) = createFFIType $ funcT typedef
     ffitypedef = makeFFIType (funcN typedef) functype
     ffifunc    = show $ Wrapper modname (funcN typedef) fromC toC (last $ funcT typedef)
     finalizerF = finalizerFunc (funcN typedef) toC (last functype)
     finalizerT = finalizerExport (funcN typedef) toC (last functype)
  in case needsFinalizer toC "?" of
        "" -> pack [ffitypedef, ffifunc]
        _  -> pack [ffitypedef, ffifunc, '\n':finalizerT, finalizerF]
  where pack = foldr (\a b -> a++'\n':b) ""
