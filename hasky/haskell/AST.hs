module AST where

import HTypes (HType(..))

data AST = Function String [AST] HType
          | Variable String HType
          | Bind     AST AST
          | Bindl    AST AST
          | Lambda   [AST] AST
          | Next     AST AST
          deriving (Eq)

instance Show AST where
    show = showAST

showAST :: AST -> String
showAST h = case h of
    Variable n _ -> ' ':n
    Lambda as bd -> ' ':parens ("\\" ++ (concat $ map showAST as) ++ " -> " ++ showAST bd)
    Bind a b     -> showAST a ++ " >>=\n    " ++ showAST b
    Bindl a b    -> showAST a ++ " =<< " ++ showAST b
    Next a b     -> showAST a ++ " >>" ++ showAST b
    Function n as _ -> ' ':parens (n ++ (concat $ map showAST as))
    where parens s = '(':s++")"

typeOf :: AST -> HType
typeOf h = case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> HIO $ typeOf b
    Bindl a b      -> HIO $ typeOf a
    Next a b       -> HIO $ typeOf b
    Lambda as b    -> typeOf b

return' :: AST -> AST
return' hast = case hast of
    Function _ _ (HIO _) -> hast
    Function _ _ ht -> Function "return" [hast] $ HIO ht
    Variable _ ht   -> Function "return" [hast] $ HIO ht
    Lambda args body -> Lambda args $ return' body
    _                -> hast

add :: AST -> AST -> AST
add hast hast' = case hast of
    Function "return" (f:[]) ft -> Function "return" [add f hast'] ft
    Function fn args ft -> Function fn (hast':args) ft
    Bind a b            -> Bind a $ add b hast'
    Bindl a b           -> Bindl a $ add b hast'
    Next a b            -> Next a $ add b hast'
    Lambda as b         -> Lambda as $ add b hast'

map' :: AST -> AST -> AST
map' f a = case typeOf f of
    (HIO ht)  -> mapM' ht
    HCWString -> mapM' HCWString
    HCArray a -> mapM' a
    ht        -> Function "map"  [mapF f a,a] (HList ht)
    where mapM' ht = Function "mapM" [mapF f a,a] (HIO (HList ht))

mapF :: AST -> AST-> AST
mapF f a = case f of
    Function fn args ft  -> case last args of
            Variable _ _ -> Function fn (init args) ft
            _            -> Lambda [a] f
    _ -> Lambda [a] f

