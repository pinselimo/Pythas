from ctypes import cdll
from functools import partial
import os.path
import tempfile
import re

from .haskell import GHC, ffi_creator
from .utils import shared_library_suffix, remove_created_files, \
                   flatten, custom_attr_getter, ffi_libs_exports
from .parser import parse_haskell

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

        ffi_libs = [self._compile(name) for name in [filename, ffi_filename]]

        remove_created_files(ffi_filename)
        return ffi_libs

    def _compile(self, name):
        parse_infos = parse_haskell(name)
        with tempfile.NamedTemporaryFile(suffix=shared_library_suffix()) as lib_file:
            self.__compiler.compile(name, lib_file.name, self.flags)
            lib = cdll.LoadLibrary(lib_file.name)
        return lib, parse_infos

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
        code = re.sub('\n[ \t]+','\n',code)
        haskell = 'module Temp where\n'+code
        compiler = Compiler()
        print(haskell)
        with tempfile.TemporaryDirectory() as dir:
            temp = os.path.join(dir,"Temp.hs")
            with open(temp,'w') as f:
                f.write(haskell)
            ffi_libs = compiler.compile(temp)

        self._ffi_libs = ffi_libs
        self._exported = ffi_libs_exports(ffi_libs)
        self.__name__ = 'SourceModule'

    def __getattr__(self, name):
        return custom_attr_getter(self, name)

    def __dir__(self):
        return list(self.__dict__) + self._exported

compiler = Compiler()

