import tempfile
from ctypes import cdll
from .haskell import GHC, ffi_creator

from .utils import get_shared_library_suffix, remove_created_files
from .haskell.parse_file import parse_haskell

class Context:
    def __init__(self):
        self.__fficreator = ffi_creator
        self.__compiler = GHC()

    @property
    def flags(self):
        return self.__compiler.custom_flags()

    def add_flag(self, flag):
        self.__compiler.custom_flags.add_flag(flag)

    def remove_flag(self, flag):
        self.__compiler.custom_flags.remove_flag(flag)

    @property
    def compiler(self):
        return self.__compiler

    def compile(self, filename):
        ffi_filename = self.__fficreator.createFileBindings(filename)
        ffi_pinfos = parse_haskell(ffi_filename)

        with tempfile.NamedTemporaryFile(suffix=get_shared_library_suffix()) as lib_file:
            self.__compiler.compile(ffi_filename, lib_file.name)
            lib = cdll.LoadLibrary(lib_file.name)
            remove_created_files(ffi_filename)

        return lib, ffi_pinfos

context = Context()

