module HTypes (HType(HFunc, HTuple, HList, HIO, HUnit), typeparsers, htype) where

import Text.Parsec ((<|>), unexpected, try, skipMany)
import Text.Parsec.Char (string, space)
import Text.Parsec.String (Parser)

data HType
   = HUnit
   | HBool
   | HChar
   | HSChar
   | HUChar
   | HShort
   | HUShort
   | HInt
   | HUInt
   | HLong
   | HULong
   | HFloat
   | HDouble
   | HInteger
   | HIO HType
   | HList HType
   | HTuple [HType]
   | HFunc [HType]
   deriving (Show, Eq)

typeparsers = [char, schar, uchar, short, ushort, int32, uint32, long, ulong, float, double, bool]
htype = foldr (<|>) (unexpected "invalid type") typeparsers

makeParser :: HType -> [String] -> Parser HType
makeParser t ss = foldr (<|>) (unexpected "invalid type") (map (try . string) ss) >> return t

bool = makeParser HBool ["Bool"]
char = makeParser HChar ["CChar","Int8"]
schar = makeParser HSChar ["CSChar","Int8"]
uchar = makeParser HUChar ["CUChar", "Word8"]
short = makeParser HShort ["CShort", "Int16"]
ushort = makeParser HUShort ["CUShort", "Word16"]
int32 = makeParser HInt ["CInt","Int32"]
uint32 = makeParser HInt ["CUInt", "Word32"]
long = makeParser HLong ["CLong", "Int64"]
ulong = makeParser HULong ["CULong", "Word64"]
float = makeParser HFloat ["Float","CFloat"]
double = makeParser HDouble ["Double","CDouble"]

