module HTypes (HType(..), htype, ffiType) where

import Text.Parsec ((<|>), unexpected, try, skipMany)
import qualified Text.Parsec.Char as PC (string)
import Text.Parsec.String (Parser)

data HType
   = HUnit
   | HBool
   | HCBool
   | HChar
   | HWChar
   | HSChar
   | HUChar
   | HShort
   | HUShort
   | HCInt
   | HCUInt
   | HLong
   | HULong
   | HLLong
   | HULLong
   | HFloat
   | HDouble
   | HCFloat
   | HCDouble
   | HInt
   | HInteger
   | HString
   | HCWString
   | HIO HType
   | HList HType
   | HTuple [HType]
   | HFunc [HType]
   | HCTuple [HType]
   | HCFunc [HType]
   | HCArray HType
   | HCList HType
   | HCPtr HType -- TODO constrain HTypes available (only Storable ones)
   deriving (Show, Eq)

ffiType :: HType -> String
ffiType ht = case ht of
    HUnit   -> "()"
    HCBool  -> "CBool"
    HChar   -> "CChar"
    HWChar  -> "HWchar"
    HSChar  -> "CSChar"
    HUChar  -> "CUChar"
    HShort  -> "CShort"
    HUShort -> "CUShort"
    HCInt   -> "CInt"
    HCUInt  -> "CUInt"
    HLong   -> "CLong"
    HULong  -> "CULong"
    HLLong  -> "CLLong"
    HULLong -> "CULLong"
    HCFloat  -> "CFloat"
    HCDouble -> "CDouble"
    HFloat   -> "Float"
    HDouble  -> "Double"
    HInt    -> "CInt"
    HInteger -> "CLLong"
    HCWString -> "CWString"
    HIO ht'  -> "IO " ++ further ht'
    HCArray ht' -> "CArray " ++ further ht'
    HCList ht'  -> "CList " ++ further ht'
    _ -> fail ("Non C-compatible type \"" ++ show ht ++ "\" in export")
    where further = (\s -> "( " ++ s ++ " )") . ffiType

htype = foldr (<|>) (unexpected "invalid type") types
 where types = [char, schar, uchar, short, ushort
               , int32, uint32, long, ulong, float
               , double, bool, integer, int, cwstring
               , cfloat, cdouble, string]

makeParser :: HType -> [String] -> Parser HType
makeParser t ss = foldr ((<|>) . try . PC.string) (unexpected "invalid type") ss
               >> return t

mp = makeParser
bool = mp HBool ["Bool"]
char = mp HChar ["CChar","Int8"]
schar = mp HSChar ["CSChar","Int8"]
uchar = mp HUChar ["CUChar", "Word8"]
short = mp HShort ["CShort", "Int16"]
ushort = mp HUShort ["CUShort", "Word16"]
int32 = mp HCInt ["CInt","Int32"]
uint32 = mp HCUInt ["CUInt", "Word32"]
long = mp HLong ["CLong", "Int64"]
ulong = mp HULong ["CULong", "Word64"]
float = mp HFloat ["Float"]
double = mp HDouble ["Double"]
cfloat = mp HCFloat ["CFloat"]
cdouble = mp HCDouble ["CDouble"]
string = mp HString ["[Char]","String"]
int = mp HInt ["Int"]
integer = mp HInteger ["Integer"]
cwstring = mp HCWString ["CWString", "Ptr CWchar"]

