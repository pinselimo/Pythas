module FFIWrapper where

import HTypes (HType(HIO))
import FFIUtils

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

argnames :: [a] -> String
argnames cs = take (length cs) ['a'..'z']

wrapString :: Wrapper -> String
wrapString w = let
       start = fname w ++ concat' args ++ equals
       qname = sp $ mname w ++ '.':fname w
       lmbds = concatNL $ lambdas w args
       args  = argnames $ argconv w
       argv  = wrapArgs w args
       resv  = wrapRes (reswrap w) (originalres w) (any isIO $ argconv w) $ qname ++ argv
       in start ++ lmbds ++ resv

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
    (Nested a b _)    -> lambda' a var maps ++ tab ++ lambda' b var (maps+1)
    (IOIn (FromC cv')) -> parens (end MapM cv') ++ next
    (Pure (FromC cv')) -> parens (return' ++ cash ++ end Map cv') ++ next
    where end m cv' = putMaps m maps++' ':cv'++[' ',var]
          next    = bind ++ '\\':var:" ->"

wrapArgs :: Wrapper -> [Char] -> String
wrapArgs w args = concat $ zipWith wrapArg (argconv w) args

wrapArg :: Convert -> Char -> String
wrapArg cv c = case cv of
    (Pure (FromC cv)) -> sp $ parens $ cv ++ v
    _                 -> v
    where v = [' ',c]

wrapRes :: Convert -> HType -> Bool -> String -> String
wrapRes (Pure (ToC cv)) ht io res = case ht of
    HIO _ -> parens (return'++ring++cv) ++ bindr ++ res
    _     -> if io
    then return' ++ cash ++ func
    else "" ++ func
    where func = cv ++ cash ++ res
wrapRes cv ht _ res = case ht of
    HIO _ -> w $ bindr ++ res
    _     -> w $ cash ++ res
    where w = wrapRes' cv 0

wrapRes' :: Convert -> Int -> String -> String
wrapRes' cv maps res = case cv of
    (Nested a b _)     -> wrapRes' a maps "" ++ bindr ++ wrapRes' b (maps+1) res
    (IOOut _ (ToC cv')) -> parens $ end MapM cv'
    (Pure (ToC cv'))    -> parens $ return' ++ ring ++ end Map cv'
    where end m cv' = putMaps m maps ++ sp cv' ++res

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
