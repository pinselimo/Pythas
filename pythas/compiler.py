"""API for Haskell compilation and binding."""

from ctypes import cdll
from functools import partial
import os.path
import tempfile
import re
import sys
from logging import getLogger

from .haskell import GHC, ffi_creator, has_stack
from .utils import (
    shared_library_suffix,
    remove_created_files,
    flatten,
    custom_attr_getter,
    ffi_libs_exports,
)
from .parser import parse_haskell

DEFAULT_FLAGS = ("-O2",)


class Compiler:
    """Interface for the compiler used to create shared libraries.

    Parameters
    ----------
    flags : Tuple[str]
        Compile time flags to append. Default value is using the "-O2" flag.

    Attributes
    ----------
    GHC_VERSION : str
        Version number string of the used GHC instance.
    flags : tuple(str)
        Flags for `compiler`.
    stack_usage : bool
        Enable the usage of stack for compilation.
        Will default to False if stack is not available.
    """

    def __init__(self, flags=DEFAULT_FLAGS):
        self.__fficreator = ffi_creator
        self._flags = list(flags)
        self._stack = has_stack()

    def copy(self):
        """Creates a new instance of ``Compiler`` from an existing one.
        Flags and ``stack_usage`` will be consistent.

        Returns
        -------
        new : Compiler
            The new Compiler instance.
        """
        new = Compiler(self.flags)
        new.stack_usage = self.stack_usage
        return new

    @property
    def GHC_VERSION(self):
        return GHC.get_version(self._stack)

    @property
    def flags(self):
        return tuple(flatten(self._flags))

    @property
    def stack_usage(self):
        return self._stack

    @stack_usage.setter
    def stack_usage(self, enabled):
        self._stack = enabled if has_stack() else False

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
        windows = sys.platform.startswith("win32")
        with tempfile.NamedTemporaryFile(
            suffix=shared_library_suffix(),
            # Deleting on windows causes access denied
            delete=not windows,
        ) as lib_file:

            GHC.compile(
                name, lib_file.name, use_stack=self.stack_usage, add_flags=self.flags
            )
            if windows:
                lib_file.close()
            lib = cdll.LoadLibrary(lib_file.name)

        return lib, parse_infos


class SourceModule:
    """Module created from inline Haskell source code.
    Will instantiate its own instance of ``Compiler``
    unless an alternative is provided.
    Other settings will not be permanently made in
    the compiler instance.

    Parameters
    ----------
    code : str
        The Haskell source code to construct the module from.
    compiler : Compiler
        Compiler instance to use settings from.
    use_stack : bool
        Use stack if available. Default value is True.
    flags : Tuple[str]
        Compile time flags to append. Default value is using the "-O2" flag.
    """

    def __init__(self, code, compiler=None, use_stack=True, flags=DEFAULT_FLAGS):
        code = re.sub("\n[ \t]+", "\n", code)
        haskell = "module Temp where\n" + code

        if compiler is None:
            compiler = Compiler(flags)
        else:
            compiler = compiler.copy()
            for flag in flags:
                compiler.add_flag(flag)
        compiler.stack_usage = use_stack

        with tempfile.TemporaryDirectory() as dir:
            temp = os.path.join(dir, "Temp.hs")
            getLogger(__name__).info("Created temporary module Temp")
            with open(temp, "w") as f:
                f.write(haskell)
            ffi_libs = compiler.compile(temp)

        self._ffi_libs = ffi_libs
        self._exported = ffi_libs_exports(ffi_libs)
        self.__name__ = "SourceModule"

    def __getattr__(self, name):
        return custom_attr_getter(self, name)

    def __dir__(self):
        return list(self.__dict__) + self._exported


compiler = Compiler()
