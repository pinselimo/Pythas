-- {#- LANGUAGE ForeignFunctionInterface -#}
module Exposed where

import Foreign.Marshal.Alloc (free)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import Foreign.C.String (CWString, peekCWString, newCWString)
import Control.Exception (Exception, throw)

import TypeParser (parseTypeDefs, TypeDef(funcN))
import ExportsParser (parseExports, parseModname)
import FFICreate (createFFI)

-- foreign export ccall createFileBindings :: CWString -> IO CWString
-- foreign export ccall freeReturnedString :: CWString -> IO ()

data HaskyExepction = ParseException ParseError
 deriving (Show)
instance Exception HaskyExepction 

createFileBindings :: CWString -> IO CWString
createFileBindings cfn = do
    fn <- peekCWString cfn
    modn <- parseFromFile parseModname fn
    modname <- case modn of
                Left e -> throw $ ParseException e
                Right modname -> return modname
    tpds <- parseFromFile parseTypeDefs fn
    typeDefs <- case tpds of 
                Left e -> throw $ ParseException e
                Right ts -> return ts 
    expts <- parseFromFile parseExports fn
    let exports = case expts of
                     Left e  -> map funcN typeDefs
                     Right e -> e
    let (fn', fc) = createFFI fn modname exports typeDefs
    writeFile fn' fc
    newCWString fn'

freeReturnedString :: CWString -> IO ()
freeReturnedString = free

main :: FilePath -> IO ()
main fp = do
 cs <- newCWString fp
 fn <- createFileBindings cs
 free cs
 free fn
