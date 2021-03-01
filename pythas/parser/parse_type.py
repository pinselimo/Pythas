"""Parse Haskell type declarations."""

import ctypes as cl
from functools import partial
from logging import getLogger
from warnings import warn

from .data import FuncInfo
from .utils import lmap, apply, strip_io, tuple_types, parse_generator, TypeWarning
from ..types import *

logger = getLogger(__name__)

EXPERIMENTAL = {
    "Int": cl.c_int,  # Experimental!
    "Integer": cl.c_longlong,  # Experimental!
}

HS2PY = {
    ### void ###
    "()": None
    ### INTEGRAL ###
    ,
    "CInt": cl.c_int32,
    "CBool": cl.c_bool,
    "Char": cl.c_char,
    "CChar": cl.c_char,
    "CSChar": cl.c_byte,
    "CUChar": cl.c_ubyte,
    "CShort": cl.c_short,
    "CUShort": cl.c_ushort,
    "CUInt": cl.c_uint32,
    "CLong": cl.c_long,
    "CULong": cl.c_ulong,
    "CSize": cl.c_size_t,
    "CWchar": cl.c_wchar,
    "CLLong": cl.c_longlong,
    "CULLong": cl.c_ulonglong
    # Usually wrapped
    ,
    "Int8": cl.c_byte,
    "Word8": cl.c_ubyte,
    "Int16": cl.c_short,
    "Word16": cl.c_ushort,
    "Int32": cl.c_int,
    "Word32": cl.c_uint,
    "Int64": cl.c_long,
    "Word64": cl.c_ulong
    ### FLOATING POINT ###
    ,
    "CDouble": cl.c_double,
    "Double": cl.c_double,  # CDouble builds a newtype over Double
    "CFloat": cl.c_float,
    "Float": cl.c_float  # CFloat builds a newtype over Float
    ### String ###
    ,
    "CString": cl.c_char_p
    # Strings cannot be freed of cl.c_wchar_p is the return type
    # however, ctypes does also not take over marshalling so
    # this weird behaviour results in memory leakage
    # Our way around this problem is to 'hide' the string behind
    # a pointer:
    ,
    "CWString": cl.POINTER(cl.c_wchar_p),
}


def simple_hs_2_py(hs_type):
    """Converts simple Haskell types to their Python equivalent.

    Parameters
    ----------
    hs_type : str
        The Haskell type.

    Returns
    -------
    pytype : type
        The Python type.

    Warnings
    --------
    TypeWarning : Custom types are an experimental feature in Pythas.
    """
    if hs_type in HS2PY:
        return HS2PY[hs_type]
    elif hs_type in EXPERIMENTAL:
        warn(
            "Usage of the type < {} > is experimental "
            "consider using a type from Foreign.C.Types "
            "instead.".format(hs_type),
            TypeWarning,
        )
        return EXPERIMENTAL[hs_type]
    else:
        logger.debug("Type {} not found within supported types".format(hs_type))
        warn(
            "Usage of custom types like < {} > "
            "is considered experimental in Pythas! "
            "It will be passed as a ctypes.c_void_p".format(hs_type),
            TypeWarning,
        )
        return cl.POINTER(None)


def hs2py(hs_type):
    """Maps Haskell to Python types.

    Parameters
    ----------
    hs_type : str
        The Haskell type.

    Returns
    -------
    pytype : type
        The Python type.
    """
    hs_type = hs_type.strip("( )")
    if hs_type == "":
        hs_type = "()"

    default = lambda _: simple_hs_2_py(hs_type)
    parse = parse_generator(
        # new_* functions are used because ctypes is strictly typed
        # to the point where two separately created linked_list
        # classes throw a TypeError. So one instance has to be used
        # throughout the function usage.
        lambda hs_inner: cl.POINTER(new_linked_list(hs2py(hs_inner))),
        lambda hs_inner: cl.POINTER(new_c_array(hs2py(hs_inner))),
        lambda hs_inner: cl.POINTER(new_tuple(list(map(hs2py, tuple_types(hs_inner))))),
        default,
        default,
    )

    return parse(hs_type)


def argtype(hs_type):
    """Parser for the argument side types of a Haskell function.

    Parameters
    ----------
    hs_type : str
        Argument side Haskell type.

    Returns
    -------
    arg : (type, callable)
        Tuple with the argument type and a callable that converts
        a conventional Python instance to an instance of the required type.
    """
    argt = hs2py(hs_type)

    default = lambda _: argt
    parse = parse_generator(
        lambda _: partial(to_linked_list, argt._type_),
        lambda _: partial(to_c_array, argt._type_),
        lambda _: partial(to_tuple, argt._type_),
        lambda _: lambda x: cl.pointer(cl.c_wchar_p(x)),  # Strings
        default,
    )

    return argt, parse(hs_type)


def restype(hs_type):
    """Parser for the result side type of a Haskell function.

    Parameters
    ----------
    hs_type : str
        Result side Haskell type.

    Returns
    -------
    res : (type, callable)
        Tuple with the result type and a callable
        that converts the type to a conventional Python type.
    """
    rtype = hs2py(hs_type)

    parse = parse_generator(
        lambda hs_inner: lambda x: lmap(restype(hs_inner)[1], from_linked_list(x)),
        lambda hs_inner: lambda x: lmap(restype(hs_inner)[1], from_c_array(x)),
        lambda hs_inner: lambda x: apply(
            lmap(restype, tuple_types(hs_inner)), from_tuple(x)
        ),
        lambda _: lambda x: x.contents.value,
        lambda _: lambda x: x,
    )

    return rtype, parse(hs_type)


def parse_type(line_nr, name, hs_type):
    """Parses the type of an FFI exported Haskell function or constant.

    Parameters
    ----------
    line_nr : int
        Line number of the parsed line.
    name : str
        Name of the function or constant.
    hs_type : str
        Type declaration of the Haskell entity.

    Returns
    -------
    func_info : FuncInfo
        Parsed information about the function.

    Raises
    ------
    TypeError : Functions as arguments are not supported
    """
    types = [t.strip() for t in hs_type.split("->")]
    if any(t.count("(") != t.count(")") for t in types):
        raise TypeError(
            'In Line Number {} - Functions as arguments like "{}" '
            "are not supported".format(line_nr, hs_type)
        )

    *inp, out = types
    io, out = strip_io(out)

    argtypes, constructors = [], []
    for i in inp:
        argt, constructor = argtype(i)
        argtypes.append(argt)
        constructors.append(constructor)
    logger.debug("Found argument types: {} for {}".format(argtypes, name))

    result_type, reconstructor = restype(out)
    logger.debug("Found result type: {} for {}".format(result_type, name))

    return FuncInfo(name, argtypes, result_type, constructors, reconstructor, hs_type)
