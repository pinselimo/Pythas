"""Init file of Pythas' haskell package."""

import os.path
from ctypes import cdll, c_wchar_p, c_voidp
from sys import platform

from .ghc import GHC
from ..utils import shared_library_suffix

dir = os.path.join(
          os.path.dirname(os.path.realpath(__file__))
        , 'ffi'
        , 'src'
        )

src = os.path.join(dir, "Exports.hs")
lib = os.path.join(dir, "libpythasffi")
if not os.path.exists( lib ):
    lib += shared_library_suffix()

GHC().compile(src, lib, _redirect=True)

ffi_creator = cdll.LoadLibrary( lib )
ffi_creator.createFileBindings.argtype = [c_wchar_p]
ffi_creator.createFileBindings.restype = c_wchar_p
ffi_creator.freeReturnedString.argtypes = [c_wchar_p]
ffi_creator.freeReturnedString.restype = c_voidp
