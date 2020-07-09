module FFIUtils where

import HTypes (HType(..))

data HAST = Function String [HAST] HType
          | Variable String HType
          | Bind     HAST HAST
          | Bindl    HAST HAST
          | Lambda   [HAST] HAST
          | Next     HAST HAST
          deriving (Eq)

instance Show HAST where
    show = showHAST

showHAST :: HAST -> String
showHAST h = case h of
    Variable n _ -> ' ':n
    Lambda as bd -> ' ':parens ("\\" ++ (concat $ map showHAST as) ++ " -> " ++ showHAST bd)
    Bind a b     -> showHAST a ++ " >>=\n    " ++ showHAST b
    Bindl a b    -> showHAST a ++ " =<< " ++ showHAST b
    Next a b     -> showHAST a ++ " >>" ++ showHAST b
    Function n as _ -> ' ':parens (n ++ (concat $ map showHAST as))

getHASTType :: HAST -> HType
getHASTType h = case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> HIO $ getHASTType b
    Bindl a b      -> HIO $ getHASTType a
    Next a b       -> HIO $ getHASTType b
    Lambda as b    -> getHASTType b

-- TODO implement this proper
return' :: HAST -> HAST
return' (Function n args ht) = Function ("return $ " ++ n) args $ HIO ht
return' (Variable n ht)      = Variable ("return $ " ++ n) $ HIO ht

id' :: HType -> HAST
id' = Function "id" []

add :: HAST -> HAST -> HAST
add hast hast' = case hast of
    Function fn args ft -> Function fn (hast':args) ft
    Bind a b            -> Bind a $ add b hast'
    Bindl a b           -> Bindl a $ add b hast'
    Next a b            -> Next a $ add b hast'
    Lambda as b         -> Lambda as $ add b hast'

map' :: HAST -> HAST -> HAST
map' f a = case getHASTType f of
    (HIO ht) -> Function "mapM" [mapF f a,a] (HIO (HList ht))
    ht       -> Function "map"  [mapF f a,a] (HList ht)

mapF :: HAST -> HAST-> HAST
mapF f a = case f of
    Function fn args ft -> Function fn [] ft
    _ -> Lambda [a] f

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
 HString -> HIO $ HCWString
 HList x -> HIO $ HCArray $ fromFFIType x
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
