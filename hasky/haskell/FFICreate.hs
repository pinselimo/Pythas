module FFICreate (createFFI) where

import HTypes (HType(..), ffiType)
import TypeParser (TypeDef(funcN, funcT))

data Convert = Pure Convert
             | IOIn Convert
             | IOOut Convert Convert
             | Nested Convert Convert
             | Free String
             | ToC String
             | FromC String
             deriving (Show, Eq)

imports = map ("import "++)
          ["Foreign.C.Types"
          ,"Foreign.C.String"
          ,"Foreign.Marshal.Utils (fromBool, toBool)"
          ,"HaskyList"
          ,"HaskyArray"
          ,"HaskyString"]

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> (FilePath, String)
createFFI fn modname exports typeDefs =
 let ffiFilename = takeWhile (/='.') fn ++ "_hasky_ffi.hs"
     ffiModname = modname ++ "_hasky_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = map (makeFFIExport modname) exportedFuncTypes
     ffiContent = "{-# LANGUAGE Foreign Function Interface #-}\n"
             ++ "module " ++ ffiModname
             ++ " where\n\n"
             ++ "import qualified " ++ modname ++ "\n\n"
             ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

 in (ffiFilename, ffiContent)

makeFFIExport :: String -> TypeDef -> String
makeFFIExport modname typedef = let
 (functype, ffifunc, finalizer) = makeFFIFunc modname typedef
 ffitypedef = makeFFIType (funcN typedef) functype
 in ffitypedef ++ "\n" ++ ffifunc ++ "\n" ++ finalizer

makeFFIFunc :: String -> TypeDef -> ([HType], String, String)
makeFFIFunc modname td = let
     (ffitype, fromC, toC) = createFFIType (funcT td)
     ffifunc = convertsToFunc modname (funcN td) fromC toC
     finalizer = makeFinalizer (funcN td) toC $ last ffitype
     in (ffitype, ffifunc, finalizer)

makeFFIType :: String -> [HType] -> String
makeFFIType funcname ccompattypes = fec ++ funcname ++ " :: " ++ functype
 where functype = argtypes ++ rettype
       argtypes = foldr (\a b -> ffiType a ++ " -> " ++ b) "" $ init ccompattypes
       rettype  = ffiType $ last ccompattypes

fec = "foreign export ccall "

-- TODO: Cover more cases
makeFinalizer :: String -> Convert -> HType -> String
makeFinalizer funcname conv ht = if needsFinalizer conv
 then finalizerExport n ht ++ "\n" ++ finalizerFunc n conv
 else ""
 where n = funcname ++ "Finalizer"


finalizerFunc :: String -> Convert -> String
finalizerFunc n freer = n ++ " = " ++ finalizerFunc' freer ++ "\n"

finalizerFunc' :: Convert -> String
finalizerFunc' freer = case freer of
 Free f -> f
 IOOut f _ -> finalizerFunc' f
 Nested a b -> finalizerFunc' a ++ 
    if isIO b
    then " mapM " ++ finalizerFunc' b
    else ""
 _ -> ""

finalizerExport :: String -> HType -> String
finalizerExport n t = fec ++ n ++ " :: " ++ ffiType t ++ " -> IO ()"

createFFIType :: [HType] -> ([HType], [Convert], Convert)
createFFIType ts =
    let fromT = map fromFFIType $ init ts
        toT   = toFFIType (any isIO fromC) $ last ts
        fromC = map fromFFIConvert $ init ts
        toC   = toFFIConvert $ last ts
    in (fromT ++ [toT], fromC, toC)

argnames :: [Convert] -> String
argnames cs = take (length cs) ['a'..'z']

convertsToFunc :: String -> String -> [Convert] -> Convert -> String
convertsToFunc modname funcname fromConvs toConv =
 let start = funcname ++ args ++ " = "
     qname = modname ++ "." ++ funcname
     args  = foldr (\a b -> ' ':a:b) " " $ argnames fromConvs
 in start ++ lambdas ++ ret ++ " $ " ++ qname ++ args
 where ret = retfunc (any isIO fromConvs) toConv
       lambdas = concat $ zipWith createLambda fromConvs $ argnames fromConvs

createLambda :: Convert -> Char -> String
createLambda c varname
 | not $ isIO c = ""
 | otherwise    = case c of
     Nested a b -> createLambda a varname ++ createLambda b varname
     IOIn (FromC c) -> c ++ ' ':varname:" >>= \\" ++ varname:" ->\n    "

retfunc :: Bool -> Convert -> String
retfunc ioIn conv
 | ioIn && not (isIO conv) = "return $ " ++ retfunc' conv
 | otherwise = retfunc' conv
 where retfunc' conv' = case conv' of
        Nested a b         -> if isIO b
           then retfunc' a ++ " $ mapM " ++ retfunc False b
           else retfunc' a ++ " $ map " ++ retfunc False b
        IOOut _ (ToC c) -> c
        Pure (ToC c)     -> c

-- FFI Export Type Construction
toFFIType :: Bool -> HType -> HType
toFFIType anyIO ht = let ht' = toFFIType' ht
        in case ht' of
             HIO _ -> ht'
             _     -> if anyIO then HIO ht' else ht'

toFFIType' :: HType -> HType
toFFIType' ht = case ht of
 HString -> HIO HCWString
 HList x -> HIO $ HCArray $ toFFIType'' x
 HTuple [x] -> undefined
 HFunc [x] -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 _ -> ht
 where toFFIType'' ht = let ht' = toFFIType' ht
                        in case ht' of
                          HIO ht'' -> ht''
                          _        -> ht'

fromFFIType :: HType -> HType
fromFFIType ht = case ht of
 HString -> HCWString
 HList x -> HCArray $ fromFFIType x
 HTuple [x] -> undefined
 HFunc [x]  -> undefined
 HInteger -> HLLong
 HInt -> HCInt
 HBool -> HCBool
 _ -> ht

-- FFI Export Type Converter Construction
toFFIConvert :: HType -> Convert
toFFIConvert ht = case ht of
 HString -> IOOut (Free "freeCWString") $ ToC "newCWString"
 HList x -> Nested (IOOut (Free "freeCArray") $ ToC "newArray") $ toFFIConvert x
 HTuple [x] -> undefined -- TODO Tuples
 HFunc  [x] -> undefined -- TODO Functions
 HInteger -> Pure $ ToC "fromIntegral"
 HInt -> Pure $ ToC "fromIntegral"
 HBool -> Pure $ ToC "fromBool"
 _ -> Pure $ ToC "id"

fromFFIConvert :: HType -> Convert
fromFFIConvert ht = case ht of
 HString -> IOIn $ FromC "peekCWString"
 HList x -> Nested (IOIn $ FromC "peekCArray") $ fromFFIConvert x
 HTuple [x] -> undefined -- TODO Tuples
 HFunc  [x] -> undefined -- TODO Functions
 HInteger -> Pure $ FromC "fromIntegral"
 HInt -> Pure $ FromC "fromIntegral"
 HBool -> Pure $ FromC "fromBool"
 _ -> Pure $ FromC "id"

isIO :: Convert -> Bool
isIO (Pure _) = False
isIO (Nested a b) = isIO a || isIO b
isIO _ = True

needsFinalizer :: Convert -> Bool
needsFinalizer (IOOut _ _) = True
needsFinalizer (Nested a b) = True
needsFinalizer _ = False
