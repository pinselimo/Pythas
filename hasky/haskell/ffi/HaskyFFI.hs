{-# LANGUAGE ForeignFunctionInterface #-}
module Exports where

import Foreign.Marshal.Alloc (free)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import Foreign.C.String (CWString, peekCWString, newCWString)
import Control.Exception (Exception, throw)

import HaskyFFI.ParseTypes (parseTypeDefs, TypeDef(funcN))
import HaskyFFI.ParseExports (parseExports, parseModname)
import HaskyFFI.FFICreate (createFFI)

foreign export ccall createFileBindings :: CWString -> IO CWString
foreign export ccall freeReturnedString :: CWString -> IO ()

newtype HaskyException = ParseException ParseError
 deriving (Show)
instance Exception HaskyException

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
