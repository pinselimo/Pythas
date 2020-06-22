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
            _  -> start ++ lmbds ++ resv

lambdas :: Wrapper -> [Char] -> [String]
lambdas w = lambdas' . zip (argconv w)

lambdas' :: [(Convert,Char)] -> [String]
lambdas' = map (uncurry lambda) . filter (\(a,_) -> needsLambda a)

needsLambda :: Convert -> Bool
needsLambda cv = case cv of
    (Nested a b _) -> needsLambda a || needsLambda b
    (IOIn _)       -> True
    _              -> False

lambda :: Convert -> Char -> String
lambda c v = lambda' c v 0

lambda' :: Convert -> Char -> Int -> String
lambda' cv var maps = case cv of
    (Nested a b _)    -> lambda' a var maps ++ '\n':tab ++ lambda' b var (maps+1)
    (IOIn (FromC cv)) -> '(':putMaps MapM maps++' ':cv++' ':var:") >>= \\"++var:" ->"
    (Pure (FromC cv)) -> '(':"return $ "++putMaps Map maps++' ':cv++' ':var:") >>= \\"++var:" ->"

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
wrapRes' cv maps res = case cv of
    (IOOut _ (ToC cv)) -> '(':putMaps MapM maps ++ ' ':cv++res++")"
    (Pure (ToC cv))    -> "(return . " ++ putMaps Map maps ++ ' ':cv++res++")"
    (Nested a b _)     -> wrapRes' a maps "" ++ " =<< " ++ wrapRes' b (maps+1) res
    (Tuple2 a b)       -> putMaps Map maps ++ "(\\(a,b) -> liftM2 toTuple2 " ++ wrapRes' a 0 " a" ++ ' ':wrapRes' b 0 " b"  ++ ")" ++ res
    (Tuple3 a b c)     -> undefined

finalizerFunc :: String -> Convert -> HType -> String
finalizerFunc n freer ft = needsFinalizer freer $ finalizerName n ++ " x = " ++ finalizerFunc' freer 0 ft " x" ++ "\n"

finalizerFunc' :: Convert -> Int -> HType -> String -> String
finalizerFunc' = finalizerFunc'' [""]

finalizerFunc'' :: [String] -> Convert -> Int -> HType -> String -> String
finalizerFunc'' peek cv maps ft var =
    let tuple2access = ["c2fst","c2snd"]
        tuple3access = ["c3fst","c3snd","c3trd"]
        nestedff x = finalizerFunc'' peek x maps ft var
    in case cv of
    Tuple2 a b          -> finalizeTuple tuple2access peek [a,b] maps ft var
    Tuple3 a b c        -> finalizeTuple tuple3access peek [a,b,c] maps ft var
    Nested a (Pure _) p -> nestedff a
    Nested a b p        -> finalizerFunc'' (p:peek) b (maps+1) ft var ++ plus ++ nestedff a
    IOOut (Free f) _    -> if maps > 0
                     then '(':'(':putMaps MapM maps ++ ' ':f++')':get maps peek var
                     else '(':putMaps MapM maps ++ ' ':f++var++")"

finalizeTuple :: [String] -> [String] -> [Convert] -> Int -> HType -> String -> String
finalizeTuple access peek cvs maps ft var = case fs of
                 []     -> ""
                 (x:[]) -> x
                 _      -> foldr (\a b-> a ++ plus ++ b) (last fs) (init fs)
    where tupleff p x = finalizerFunc'' (p:peek) x maps ft var
          fs = map (uncurry tupleff) $ filter (\(_,a)->needsFinalizer' a) $ zip access cvs

get :: Int -> [String] -> String -> String
get 0 _  var   = var ++ ")"
get maps (peek:ps) var = " =<< " ++ putMaps MapM (maps-1) ++ ' ':peek ++ get (maps-1) ps var

plus = '\n':tab++" >> "
