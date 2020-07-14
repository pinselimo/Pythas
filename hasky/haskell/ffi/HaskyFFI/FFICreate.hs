module HaskyFFI.FFICreate (createFFI) where

import System.FilePath.Posix (dropExtension)

import HaskyFFI.ParseTypes (TypeDef(funcN, funcT))
import HaskyFFI.FFIType (createFFIType, makeFFIType, finalizerExport)
import HaskyFFI.Wrapper (wrap)
import HaskyFFI.Finalizer (maybeFinalizerFunc)

imports = map ("import "++)
          ["Foreign.C.Types"
          ,"Foreign.Marshal.Utils (fromBool, toBool)"
          ,"Foreign.Marshal.Alloc (free)"
          ,"HaskyList"
          ,"HaskyArray"
          ,"HaskyString"]

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> (FilePath, String)
createFFI fn modname exports typeDefs =
 let ffiFilename = dropExtension fn ++ "_hasky_ffi.hs"
     ffiModname = modname ++ "_hasky_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = concat $ map (makeFFIExport modname) exportedFuncTypes
     ffiContent = "{-# LANGUAGE ForeignFunctionInterface #-}\n"
             ++ "module " ++ ffiModname
             ++ " where\n\n"
             ++ "import qualified " ++ modname ++ "\n\n"
             ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

 in (ffiFilename, ffiContent)

makeFFIExport :: String -> TypeDef -> [String]
makeFFIExport modname typedef = let
     functype = createFFIType $ funcT typedef
     ffitypedef = makeFFIType (funcN typedef) functype
     ffifunc    = wrap modname (funcN typedef) (funcT typedef)
     finalizerF = maybeFinalizerFunc (funcN typedef) (last $ funcT typedef)
     finalizerT = finalizerExport (funcN typedef) (last functype)
  in case finalizerF of
     Just finalizer -> ["",ffitypedef, ffifunc, "", finalizerT, finalizer]
     Nothing        -> ["",ffitypedef, ffifunc]

