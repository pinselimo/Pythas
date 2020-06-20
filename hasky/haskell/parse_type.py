import ctypes as cl
from functools import partial

from ..types import *
from ..parser import FuncInfo

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
        'CWString':cl.c_wchar_p,
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
    ll = hs_type.find('CList ')
    arr = hs_type.find('CArray ')
    if ll+1 and (ll < arr or arr < 0): ## Linked List first
        cls = cl.POINTER(new_linked_list(hs2py(hs_type[ll+len('CList '):])))
    elif arr+1 and (arr < ll or ll < 0): ## array first
        cls = cl.POINTER(new_c_array(hs2py(hs_type[arr+len('CArray '):])))
    else: # neither linked list nor array
        cls = simple_hs_2_py(hs_type)
    return cls

def argtype(hs_type):
    '''
    returns: tuple : (type of argument, constructor)
    '''
    argt = hs2py(hs_type)

    ll = hs_type.find('CList ')
    if ll < 0:
        arr = hs_type.find('CArray ')
        if arr < 0:
            return argt, argt
        else:
            return argt, partial(to_c_array, argt._type_)
    else:
        return argt, partial(to_linked_list, argt._type_)

def restype(hs_type):
    '''
    returns: tuple : (type of result, reconstructor, bool: needsFinalizer)
    '''
    ll = hs_type.find('CList ')
    restype = hs2py(hs_type)
    if ll < 0:
        arr = hs_type.find('CArray ')
        if arr < 0:
            return restype, lambda x:x
        else:
            return restype, from_c_array
    else:
        return restype, from_linked_list

def strip_io(tp):
    '''
    IO is somewhat disregarded in the FFI exports. IO CDouble
    looks the same as CDouble from Python's side. So we remove
    the monadic part from our type to process the rest.
    '''
    io = tp.find('IO ')
    if io < 0:
        return '', tp
    else:
        return 'IO ',tp[io+3:]

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

    restp, reconstructor = restype(out)

    return FuncInfo(name, argtypes, restp, constructors, reconstructor)
