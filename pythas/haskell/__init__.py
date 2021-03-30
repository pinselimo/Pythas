"""The ``pythas.haskell`` module contains a wrapper around GHC and all the Haskell source Pythas is based on. After the installation of Pythas, the Haskell source files need to be compiled first. Subsequent imports of Pythas will **not** need any compilation and thus result in quicker import times.

Haskell source documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These parts of Python are split into three Haskell packages:

 * C-structs

 This package may be useful in many contexts and is therefore available on Hackage.  It provides types representing structs as known from the C programming language in Haskell.

 * Pythas-Types

 The types in Pythas-Types are based on the struct types from C-structs. They provide a foundation for the interfacing over the FFIs of both Haskell and Python.

 * Pythas-FFI

 Parsing, wrapping and code generation are contained in here. First an AST of the type declarations is parsed. This AST is then extended to utilize Pythas-Types compatible input and output data. The extended AST is compiled into a Haskell source module and its path returned.
"""

import os.path
from ctypes import cdll, c_wchar_p, c_voidp
from sys import platform

from .ghc import GHC, has_stack
from ..utils import shared_library_suffix, building_docs

dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), "ffi", "src")

src = os.path.join(dir, "Exports.hs")
lib = os.path.join(dir, "libpythasffi")
if not os.path.exists(lib):
    lib += shared_library_suffix()

if building_docs():
    import mock

    ffi_creator = mock.Mock()
else:
    GHC.compile(src, lib, _redirect=True)

    ffi_creator = cdll.LoadLibrary(lib)
    ffi_creator.createFileBindings.argtype = [c_wchar_p]
    ffi_creator.createFileBindings.restype = c_wchar_p
    ffi_creator.freeReturnedString.argtypes = [c_wchar_p]
    ffi_creator.freeReturnedString.restype = c_voidp
