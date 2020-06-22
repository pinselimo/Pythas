module FFIWrapper where

import HTypes (HType(HIO))
import FFIUtils

tab = "    "

type Modname = String
type Funcname = String

data Wrapper = Wrapper
    { mname :: Modname
    , fname :: Funcname
    , argconv :: [Convert]
    , reswrap :: Convert
    , originalres :: HType
    }

instance Show Wrapper where
  show = wrapString

argnames :: [Convert] -> String
argnames cs = take (length cs) ['a'..'z']

wrapString :: Wrapper -> String
wrapString w = let
       start = fname w ++ foldr (\a b->' ':a:b) " " args ++ " = "
       qname = ' ':mname w ++ '.':fname w
       lmbds = foldr (\a b -> a ++ '\n':tab ++ b) "" $ lambdas w args
       args  = argnames $ argconv w
       argv  = wrapArgs w args
       resv  = wrapRes (reswrap w) (originalres w) (any isIO $ argconv w) $ qname ++ argv
       in case lmbds of 
            "" -> start ++ resv
            _  -> start ++ lmbds ++ tab ++ resv

lambdas :: Wrapper -> [Char] -> [String]
lambdas w = lambdas' . zip (argconv w)

lambdas' :: [(Convert,Char)] -> [String]
lambdas' = map (uncurry lambda) . filter (\(a,_) -> needsLambda a)

needsLambda :: Convert -> Bool
needsLambda (Nested a b _) = needsLambda a || needsLambda b
needsLambda (IOIn _) = True
needsLambda _ = False

lambda :: Convert -> Char -> String
lambda c v = lambda' c v 0

lambda' :: Convert -> Char -> Int -> String
lambda' (Nested a b _) var maps = lambda' a var maps ++ '\n':tab ++ lambda' b var (maps+1)
lambda' (IOIn (FromC cv)) var maps = '(':putMaps MapM maps++' ':cv++' ':var:") >>= \\"++var:" ->"
lambda' (Pure (FromC cv)) var maps = '(':"return $ "++putMaps Map maps++' ':cv++' ':var:") >>= \\"++var:" ->"

wrapArgs :: Wrapper -> [Char] -> String
wrapArgs w args = concat $ zipWith wrapArg (argconv w) args

wrapArg :: Convert -> Char -> String
wrapArg (Pure (FromC cv)) c = ' ':'(':cv ++ ' ':c:")"
wrapArg _ c = [' ',c]

wrapRes :: Convert -> HType -> Bool -> String -> String
wrapRes (Pure (ToC cv)) ht io res = case ht of
    HIO _ -> "(return . "++cv++") =<< " ++ res
    _     -> if io
    then "return $ " ++ func
    else "" ++ func
    where func = cv ++ " $ " ++ res
wrapRes cv ht _ res = case ht of
    HIO _ -> w $ " =<< " ++ res
    _     -> w $ " $ " ++ res
    where w = wrapRes' cv 0

wrapRes' :: Convert -> Int -> String -> String
wrapRes' (IOOut _ (ToC cv)) maps res = '(':putMaps MapM maps ++ ' ':cv++res++")"
wrapRes' (Pure (ToC cv)) maps res = "(return . " ++ putMaps Map maps ++ ' ':cv++res++")"
wrapRes' (Nested a b _) maps res = wrapRes' a maps "" ++ " =<< " ++ wrapRes' b (maps+1) res

finalizerFunc :: String -> Convert -> HType -> String
finalizerFunc n freer ft = needsFinalizer freer $ finalizerName n ++ " x = " ++ finalizerFunc' freer 0 ft " x" ++ "\n"

finalizerFunc' :: Convert -> Int -> HType -> String -> String
finalizerFunc' = finalizerFunc'' [""]

finalizerFunc'' :: [String] -> Convert -> Int -> HType -> String -> String
finalizerFunc'' peek cv maps ft var = case cv of
    (Nested a (Pure _) p) -> finalizerFunc'' peek a maps ft var
    (Nested a b p) -> finalizerFunc'' (p:peek) b (maps+1) ft var ++ " >> " ++ finalizerFunc'' peek a maps ft var
    (IOOut (Free f) _) -> if maps > 0
                     then '(':'(':putMaps MapM maps ++ ' ':f++')':get maps peek var
                     else '(':putMaps MapM maps ++ ' ':f++var++")"

get :: Int -> [String] -> String -> String
get 0 _  var   = var ++ " )"
get maps (peek:ps) var = " =<< " ++ putMaps MapM (maps-1) ++ ' ':peek ++ get (maps-1) ps var

