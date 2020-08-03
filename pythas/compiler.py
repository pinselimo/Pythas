from ctypes import cdll
from functools import partial
import os.path
import tempfile

from .haskell import GHC, ffi_creator
from .utils import shared_library_suffix, remove_created_files, flatten, custom_attr_getter
from .haskell.parse_file import parse_haskell

class Compiler:
    def __init__(self):
        self.__fficreator = ffi_creator
        self.__compiler = GHC()
        self._custom_flags = Flags()

    @property
    def compiler(self):
        return self.__compiler

    @property
    def flags(self):
        return self._custom_flags()

    def add_flag(self, flag):
        self._custom_flags.add_flag(flag)

    def remove_flag(self, flag):
        self._custom_flags.remove_flag(flag)

    def compile(self, filename):
        ffi_filename = self.__fficreator.createFileBindings(filename)
        ffi_pinfos = parse_haskell(ffi_filename)

        with tempfile.NamedTemporaryFile(suffix=shared_library_suffix()) as lib_file:
            self.__compiler.compile(ffi_filename, lib_file.name, self.flags)
            lib = cdll.LoadLibrary(lib_file.name)
            remove_created_files(ffi_filename)

        return lib, ffi_pinfos

class Flags:
    def __init__(self):
        self._flags = list()

    def __call__(self):
        return tuple(flatten(self._flags))

    def add_flag(self, flag):
        if flag not in self._flags:
            self._flags.append(flag)

    def remove_flag(self, flag):
        if flag in self._flags:
            self._flags.remove(flag)

class SourceModule:
    def __init__(self, code):
        haskell = 'module Temp where\n'+code
        compiler = Compiler()
        with tempfile.TemporaryDirectory() as dir:
            temp = os.path.join(dir,"Temp.hs")
            with open(temp,'w') as f:
                f.write(haskell)
            lib, ffi_pinfos = compiler.compile(temp)

        self._ffi_libs = [(lib, ffi_pinfos)]
        self._exported = list(ffi_pinfos.exported_ffi)
        self.__name__ = 'SourceModule'

    def __getattr__(self, name):
        return custom_attr_getter(self, name)

    def __dir__(self):
        return list(self.__dict__) + self._exported

compiler = Compiler()

