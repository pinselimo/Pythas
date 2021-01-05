"""API for Haskell compilation and binding."""

from ctypes import cdll
from functools import partial
import os.path
import tempfile
import re
import sys

from .haskell import GHC, ffi_creator
from .utils import shared_library_suffix, remove_created_files, \
                   flatten, custom_attr_getter, ffi_libs_exports
from .parser import parse_haskell

class Compiler:
    """Interface for the compiler used to create shared libraries.

    Attributes
    ----------
    ghc : GHC
        More concrete implementation of the actual compiler.
    flags : tuple(str)
        Flags for `compiler`.
    """
    def __init__(self):
        self.__fficreator = ffi_creator
        self.__compiler = GHC()
        self._flags = list()

    @property
    def ghc(self):
        return self.__compiler

    @property
    def flags(self):
        return tuple(flatten(self._flags))

    def add_flag(self, flag):
        """Adds a flag to `flags`.

        Parameters
        ----------
        flag : str
            A valid compile time flag.
        """
        if flag not in self._flags:
            self._flags.append(flag)

    def remove_flag(self, flag):
        """Removes a flag from `flags`.

        Parameters
        ----------
        flag : str
            A flag contained within `flags`.
        """
        if flag in self._flags:
            self._flags.remove(flag)

    def stack_usage(self, enabled):
        """Enable the usage of stack for compilation.
        Will default to False if stack is not available.

        Parameters
        ----------
        enabled : bool
            True if stack should be enabled.

        Returns
        -------
        enabled : bool
            True if stack is now enabled.
        """
        return self.__compiler.stack_usage(enabled)

    def compile(self, filename):
        """Creates an FFI file, compiles and links it against the
        Python runtime.

        Parameters
        ----------
        filename : str
            Pathlike object to a Haskell source file.

        Returns
        -------
        ffi_libs : [(ctypes.CDLL, pythas.parser.data.ParseInfo)]
            List of tuples of linked libraries and their respective parsed infos.
        """
        ffi_filename = self.__fficreator.createFileBindings(filename)

        ffi_libs = [self._compile(name) for name in [filename, ffi_filename]]

        remove_created_files(ffi_filename)
        return ffi_libs

    def _compile(self, name):
        """Compiles an FFI file, links its library and parses for infos.

        Parameters
        ----------
        name : str
            Pathlike object to a Haskell file containing FFI exports.

        Returns
        -------
        (lib, parse_infos) : The linked library and its parsed infos
        """
        parse_infos = parse_haskell(name)
        windows = sys.platform.startswith('win32')
        with tempfile.NamedTemporaryFile(
                suffix = shared_library_suffix(),
                # Deleting on windows causes access denied
                delete = not windows
                ) as lib_file:

            self.__compiler.compile(name, lib_file.name, self.flags)
            if windows: lib_file.close()
            lib = cdll.LoadLibrary(lib_file.name)

        return lib, parse_infos

class SourceModule:
    """Wrapper for runtime created Haskell source.

    Parameters
    ----------
    code : str
        The Haskell source code to wrap.
    """
    def __init__(self, code):
        code = re.sub('\n[ \t]+','\n',code)
        haskell = 'module Temp where\n'+code
        compiler = Compiler()

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

