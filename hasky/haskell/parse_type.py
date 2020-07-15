import ctypes as cl
from functools import partial

from ..types import *
from ..parser import FuncInfo
from .utils import lmap, apply, strip_io, tuple_types, parse_generator

HS2PY = {
        ### void ###
        '()':None,

        ### INTEGRAL ####
        'CInt':cl.c_int32,
        'CBool':cl.c_bool,
        'CChar':cl.c_char,
        'CSChar':cl.c_byte,
        'CUChar':cl.c_ubyte,
        'CShort':cl.c_short,
        'CUShort':cl.c_ushort,
        'CUInt':cl.c_uint32,
        'CLong':cl.c_long,
        'CULong':cl.c_ulong,
        'CSize':cl.c_size_t,
        'CWchar':cl.c_wchar,
        'CLLong':cl.c_longlong,
        'CULLong':cl.c_ulonglong,

        ### FLOATING POINT ###
        'CDouble':cl.c_double,
        'Double':cl.c_double, # CDouble builds a newtype over Double
        'CFloat':cl.c_float,
        'Float':cl.c_float, # CFloat builds a newtype over Float

        ### String ###
        'CString':cl.c_char_p,
        # Strings cannot be freed of cl.c_wchar_p is the return type
        # however, ctypes does also not take over marshalling so
        # this weird behaviour results in memory leakage
        # Our way around this problem is to 'hide' the string behind
        # a pointer:
        'CWString':cl.POINTER(cl.c_wchar_p)
    }

def simple_hs_2_py(hs_type):
    if hs_type in HS2PY:
        return HS2PY[hs_type]
    else:
        raise TypeError('Non-simple type "{}" cannot be used with Hasky'.format(hs_type))

def hs2py(hs_type):
    '''
    HS2PY maps which Haskell type will end up as which
    ctypes type at Python's side

    ctypes is strictly typed to a point where you cannot use
    two seperately created linked list classes with the same type -.-
    '''
    hs_type = hs_type.strip('( )')
    if hs_type == '':
        hs_type = '()'
    default = lambda _:simple_hs_2_py(hs_type)
    def hs_tuple(f):
        return lambda hs_inner:cl.POINTER(f(*map(hs2py,tuple_types(hs_inner))))
    parse = parse_generator(
            lambda hs_inner:cl.POINTER(new_linked_list(hs2py(hs_inner))),
            lambda hs_inner:cl.POINTER(new_c_array(hs2py(hs_inner))),
            hs_tuple(new_tuple2), hs_tuple(new_tuple3), hs_tuple(new_tuple4),
            default,default)
    return parse(hs_type)

def argtype(hs_type):
    '''
    returns: tuple : (type of argument, constructor)
    '''
    argt = hs2py(hs_type)
    default = lambda _: argt
    parse = parse_generator(
            lambda _: partial(to_linked_list, argt._type_),
            lambda _: partial(to_c_array, argt._type_),
            default, default, default, # Tuples
            lambda _: lambda x: cl.pointer(cl.c_wchar_p(x)), # Strings
            default
            )
    return argt, parse(hs_type)

def restype(hs_type):
    '''
    returns: tuple : (type of result, reconstructor)
    '''
    rtype = hs2py(hs_type)
    def restup(f):
        return lambda hs_inner:lambda x:apply(lmap(restype,tuple_types(hs_inner)), f(x))
    parse = parse_generator(
        lambda hs_inner:lambda x:lmap(restype(hs_inner)[1],from_linked_list(x)),
        lambda hs_inner:lambda x:lmap(restype(hs_inner)[1],from_c_array(x)),
        restup(from_tuple2), restup(from_tuple3), restup(from_tuple4),
        lambda _:lambda x:x.contents.value,
        lambda _:lambda x:x)
    return rtype,parse(hs_type)

def parse_type(name, hs_type):
    types = [t.strip() for t in hs_type.split('->')]
    if any(('(' in t) != (')' in t) for t in types):
        raise TypeError('Functions as arguments like "{}" not supported'.format(hs_type))

    *inp,out = types
    io, out = strip_io(out)

    argtypes = list()
    constructors = list()
    for i in inp:
        argt, constructor = argtype(i)
        argtypes.append(argt)
        constructors.append(constructor)

    restp, reconstructor= restype(out)

    return FuncInfo(
              name, argtypes, restp, constructors
            , reconstructor, hs_type
            )

