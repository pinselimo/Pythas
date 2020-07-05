module FFIUtils where

import HTypes (HType(..))

data HAST = Function String [HAST] HType
          | Variable String HType
          | Bind     HAST HAST
          | Lambda   [HAST] HAST
          deriving (Eq)

instance Show HAST where
    show = showHAST

showHAST :: HAST -> String
showHAST h = case h of
    (Variable n _) -> ' ':n
    (Lambda as bd) -> ' ':parens ("\\" ++ (concat $ map showHAST as) ++ " ->\n   " ++ showHAST bd)
    (Bind a b)     -> showHAST a ++ " >>=" ++ showHAST b
    (Function n as _) -> ' ':parens (n ++ (concat $ map showHAST as))

getHASTType :: HAST -> HType
getHASTType h = case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> HIO $ getHASTType b
    Lambda as b    -> getHASTType b

-- TODO implement this proper
return' :: HAST -> HAST
return' (Function n args ht) = Function ("return $ " ++ n) args $ HIO ht
return' (Variable n ht)      = Variable ("return $ " ++ n) $ HIO ht

id' :: HType -> HAST
id' = Function "id" []

add :: HAST -> HAST -> HAST
add hast hast' = case hast of
    (Function fn args ft) -> Function fn (hast':args) ft
    (Bind a b)            -> Bind a $ add b hast'
    (Lambda as b)         -> Lambda as $ add b hast'

map' :: HAST -> HAST
map' a = case getHASTType a of
    (HIO ht) -> Function "mapM" [a] (HIO (HList ht))
    _        -> case a of
        (Function n (a':as) ht) -> let -- remove last add and map over it
            (Function conv (x:_) t) = a'
            in add (Function n as ht) $ Function "map" [Function conv [] t,x] (HList ht)
        _ -> a

finalize :: TypeDef -> HAST
finalize = undefined

finalizerName = (++"Finalizer")

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
 HDouble -> HCDouble
 HFloat -> HCFloat
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
 HDouble -> HCDouble
 HFloat -> HCFloat
 _ -> ht

isIO :: HType -> Bool
isIO (HIO _) = True
isIO _ = False

-- Writer functions
sp s = ' ':s
tab = "\n    "
bindr = " =<< "
bind = " >>= "
cash = " $ "
ring = " . "
equals = " = "
concat' :: [Char] -> String
concat' = foldr (\a b->' ':a:b) ""
concatNL :: [String] -> String
concatNL = foldr (\a b -> a ++ tab ++ b) ""
parens s = '(':s++")"
