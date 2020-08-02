import tempfile
from ctypes import cdll

from .utils import get_shared_library_suffix, remove_created_files
from .haskell.parse_file import parse_haskell

class Context:
    def __init__(self, ffi_creator, compiler):
        self.__fficreator = ffi_creator
        self.__compiler = compiler

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

