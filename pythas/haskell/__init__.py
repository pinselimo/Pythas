import os.path
from ctypes import cdll, c_wchar_p, c_voidp
from sys import platform

from .ghc import GHC

ghc = GHC()
dir = os.path.dirname(os.path.realpath(__file__))
dir = os.path.join( dir, "ffi", "src" )
src = os.path.join( dir, "Exports.hs")
lib = os.path.join( dir, "libpythasffi")
if not os.path.exists( lib ):
    if platform.startswith('linux'):
        lib += '.so'
    elif platform.startswith('win32'):
        lib += '.dll'

ghc.compile(src, lib, redirect=True)

ffi_creator = cdll.LoadLibrary( lib )
ffi_creator.newFileBindings.argtypes = [c_wchar_p, c_wchar_p]
ffi_creator.newFileBindings.argtypes = [c_wchar_p, c_wchar_p]
ffi_creator.createFileBindings.argtype = [c_wchar_p]
ffi_creator.createFileBindings.restype = c_wchar_p
ffi_creator.freeReturnedString.argtypes = [c_wchar_p]
ffi_creator.freeReturnedString.restype = c_voidp

