module FFIFinalizer where

import HTypes (HType(HIO))
import FFIUtils

finalizerFunc :: String -> Convert -> HType -> String
finalizerFunc n freer ft = if needsFinalizer freer
                         then finalizerName n ++ arg ++ equals ++
                              finalizerFunc' [""] freer 0 ft arg ++ "\n"
                         else ""
                         where arg = sp "x"

finalizerFunc' :: [String] -> Convert -> Int -> HType -> String -> String
finalizerFunc' peek cv maps ft var = case cv of
    (Nested a (Pure _) p) -> finalizerFunc' peek a maps ft var
    (Nested a b p) -> finalizerFunc' (p:peek) b (maps+1) ft var
                      ++ tab ++ " >> "
                      ++ finalizerFunc' peek a maps ft var
    (IOOut (Free f) _) -> if maps > 0
                     then parens $ parens (putMaps MapM maps ++ sp f) ++ getAt maps peek var
                     else parens $ putMaps MapM maps ++ sp f ++var

getAt :: Int -> [String] -> String -> String
getAt 0    _         var = var
getAt _    []        var = var
getAt maps (peek:ps) var = bindr
                      ++ putMaps MapM (maps-1)
                      ++ sp peek
                      ++ getAt (maps-1) ps var
