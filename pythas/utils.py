"""Utility module for the Pythas API."""

from ctypes import _SimpleCData, _Pointer
from collections import abc
from shutil import which
from functools import reduce
from logging import getLogger
import os
import sys

fst = lambda x: x[0]
snd = lambda x: x[1]
thd = lambda x: x[2]


def flatten(seq):
    """Creates a list of all basal elements of a nested sequence.

    Parameters
    ----------
    seq : Sequence
        An arbitrarily deep nested sequence.

    Returns
    -------
    flatseq : List
        List of all basal elements of `seq`.
    """

    def flat(ts):
        if isinstance(ts, abc.Iterable) and not isinstance(ts, str):
            for t in ts:
                yield from flat(t)
        else:
            yield ts

    return list(flat(seq))


class PythasFunc:
    """Wrapper class for functions imported from a compiled Haskell module.

    Parameters
    ----------
    name : str
        The name of the function.
    func_info : parser.data.FuncInfo
        Parsed information about the function.
    funcPtr : ctypes._FuncPtr
        The pointer to the function.
    destructorPtr : ctypes._FuncPtr
        Pointer to the function releasing any memory allocated by the function in `funcPtr`.
        None if no destruction is required.
    """

    def __init__(self, name, func_info, funcPtr, destructorPtr=None):
        self.__name__ = name

        self.argtypes = func_info.argtypes
        self.constructors = func_info.constructors
        self.reconstructor = func_info.reconstructor
        self.restype = func_info.restype

        self._funcPtr = funcPtr
        self._funcPtr.argtypes = list(func_info.argtypes)
        self._funcPtr.restype = func_info.restype

        self.destructor = destructorPtr

    def __call__(self, *args):
        args = flatten([constr(a) for constr, a in zip(self.constructors, args)])
        getLogger(self.__name__).debug("Calling function with args: {}".format(args))
        val = self._funcPtr(*args)
        res = self.reconstructor(val)

        if self.destructor:
            getLogger(self.__name__).debug("Calling destructor")
            self.destructor(val)
        return res


def find_source(name, path, extension=".hs"):
    """Discovery function for Haskell modules.

    Parameters
    ----------
    name : str
        The name of the module.
    path : str or path-like object
        The path of the source directory.
    extension : str
        The file extension to be used. Default value is '.hs'.

    Returns
    -------
    source : List[str]
        List containing the source file path for module `name`.
        Empty list if it couldn't be discovered.
    """
    hsName = name + extension
    for file in os.listdir(path):
        if file.lower() == hsName:
            return [os.path.join(path, file)]
    else:
        return []


def custom_attr_getter(obj, name):
    """Pythas modules' __getattribute__ instance.
    Retrieves foreign functions imported from Haskell
    modules by their name. Creates a `PythasFunc`
    instance and calls the finalizer function if given.

    Parameters
    ----------
    obj : module
        The module of the overridden __getattribute__ method.
    name : str
        The name to be retrieved from the module.

    Returns
    -------
    res : Either a constant or an instance of PythasFunc

    Raises
    ------
    AttributeError : No corresponding foreign import could be found for `name`.
    """
    logger = getLogger(obj.__name__)

    ffi_libs = obj._ffi_libs
    not_found = AttributeError(
        "{} object has no attribute {} "
        "and no Haskell module containing it."
        "".format(obj.__name__, repr(name))
    )
    for lib, info in ffi_libs:
        logger.debug("Looking for {} in {}".format(name, info.name))
        if name in info.exported_ffi:
            func = getattr(lib, name)
            func_infos = info.func_infos[name]

            if is_constant(func_infos):
                logger.debug("Solving constant {}".format(name))
                res = func()
            else:
                finalizerName = name + "Finalizer"
                if finalizerName in info.exported_ffi:
                    destrPtr = getattr(lib, finalizerName)
                else:
                    destrPtr = None
                logger.debug("Wrapping function {}".format(name))
                res = PythasFunc(name, func_infos, func, destrPtr)

            return res
    else:
        raise not_found


# TODO : remove legacy function
def check_ctype_seq(seq):
    def _check(seq):
        return any(
            not isinstance(e, _SimpleCData)
            if not isinstance(e, abc.Iterable)
            else _check(e)
            for e in seq
        )

    if not _check(seq):
        raise TypeError("Only sequences of <ctypes._SimpleCData> allowed.")
    else:
        return seq


def is_constant(func_infos):
    """Checks if an imported function is actually a pure constant.
    Constants that have to go be wrapped in the IO monad for head storage
    (like lists) are considered impure.

    Parameters
    ----------
    func_infos : parser.data.FuncInfo
        Parsed information about the function.

    Returns
    -------
    is_constant : bool
        True if the function is actually a constant.
    """
    return not (func_infos.argtypes or "IO" in func_infos.htype)


def check_has_ghc():
    """Looks for a valid GHC or STACK installation is available
    and in the PATH variable.

    Raises
    ------
    ImportError : No means of Haskell compilation is available.
    """
    if not (which("ghc") or which("stack")):
        raise ImportError(
            "No GHC found. Please install either Stack or GHC "
            "and make sure that either is in your $PATH."
        )


def shared_library_suffix():
    """Defines the file suffix for shared library files
    depending on the host operating system.

    Returns
    -------
    suffix : str
        The correct suffix containing a dot like: '.so' .
    """
    if sys.platform.startswith("linux"):
        return ".so"
    elif sys.platform.startswith("win32"):
        return ".dll"
    elif sys.platform.startswith("darwin"):
        return ".dylib"


def remove_created_files(filename):
    """Removes all files created by `Pythas` during
    the compilation process.

    Parameters
    ----------
    filename : str or path-like object
        Path to the Haskell module source file.
    """
    path, fname = os.path.split(filename)
    basename, _ = os.path.splitext(fname)
    for ext in (".hs", ".hi", ".o", "_stub.h"):
        fn = os.path.join(path, basename + ext)
        getLogger(__name__).info("Removing: {}".format(fn))
        os.remove(fn)


def ffi_libs_exports(ffi_libs):
    """Collects the exported function names of
    a sequence of FFI library tuples. (See below)

    Parameters
    ----------
    ffi_libs : Sequence of tuples of the DLL import and its parser.data.ParseInfo
        All imported modules and their parsed information.

    Returns
    -------
    exports : Set[str]
        Set with all the exported names of the imported modules.
    """
    return reduce(lambda a, b: a | b[1].exported_ffi, ffi_libs, set())


def building_docs():
    return os.getenv("READTHEDOCS") == "True"
