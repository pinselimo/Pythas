{#- LANGUAGE ForeignFunctionInterface -#}
module Exposed where

import Foreign.Marshal.Alloc (free)
import Text.Parsec.String (parseFromFile)
import Foreign.C.String (CWString, peekCWString, newCWString)

import ExportsParser (parseExports, parseModname)
import FFICreate (createFFI)

foreign export ccall createFileBindings :: CWString -> IO CWString
foreign export ccall freeReturnedString :: CWString -> IO ()

createFileBindings :: CWString -> IO CWString
createFileBindings cfn = do
    fn <- peekCString cfn
    modname  <- parseFromFile parseModname fn
    exports  <- parseFromFile parseExports fn
    typeDefs <- parseFromFile parseTypeDefs fn
    let (fn', fc) = createFFI fn modname exports typeDefs
    writeFile fn' fc
    newCWString fn'

freeReturnedString :: CWString -> IO ()
freeReturnedString = free
