"""Data container classes for parsing."""

from collections import namedtuple

_ParseInfo = namedtuple(
    "ParseInfo", ["name", "dir", "exported_mod", "exported_ffi", "func_infos"]
)


class ParseInfo(_ParseInfo):
    """Container class for informations about a Haskell source module.

    Attributes
    ----------
    name : str
        Module name
    dir : str
        Pathlike object to the source file.
    exported_mod : set(str)
        Exported functions according to the module header.
    exported_ffi : set(str)
        Functions with an FFI export.
    func_infos : dict(str, FuncInfo)
        Dictionary mapping function names to their respective FuncInfo instance.
    """

    pass


_FuncInfo = namedtuple(
    "FuncInfo",
    ["name", "argtypes", "restype", "constructors", "reconstructor", "htype"],
)


class FuncInfo(_FuncInfo):
    """Container class for informations about a Haskell function.

    Attributes
    ----------
    name : str
        The name of the function.
    argtypes : list(ctype)
        List with types of the function arguments.
    restype : ctype
        Return type of the function.
    constructors : list(callable)
        List of functions converting Python types to their respective `argtypes`.
    reconstructor : callable
        Callable converting the `restype` back to a native Python type and releasing any memory allocated in the transferring process.
    htype : str
        Type as given by the Haskell FFI export.
    """

    pass
