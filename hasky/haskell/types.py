import ctypes as cl
from functools import partial

from ..types import new_linked_list, to_linked_list, from_linked_list, to_c_array, from_c_array

HS2PY = {
        ### void ###
        '()':None,

        ### INTEGRAL ####
        'CInt':cl.c_int32,
        'Int':cl.c_int, # fascinatingly works so far
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
        'Double':cl.c_double, # fascinatingly works so far
        'CFloat':cl.c_float,

        ### String ###
        'CString':cl.c_char_p,
    }

def hs2hs(hs_type):
    '''
    HS2HS defined which Haskell input type
    requires which Haskell side type conversion
    '''
    inner = hs_type.strip('[]').strip()
    if len(inner) <= len(hs_type)-2:
        # We found a list!
        return ("(CList {})".format(inner))

def hs2py(hs_type):
    '''  
    HS2PY maps which Haskell type will end up as which
    ctypes type at Python's side

    ctypes is strictly typed to a point where you cannot use
    two seperately initialized linked lists with the same type -.-
    '''
    ll = hs_type.find('CList ')
    if ll < 0:
        arr = hs_type.find('CArray ')
        if arr < 0:
            try:
                cls = HS2PY[hs_type.strip()]
            except KeyError:
                cls = cl.c_int
        else:
            cls = cl.POINTER(hs2py(hs_type[arr+len('CArray '):]))
    else:
        cls = cl.POINTER(new_linked_list(hs2py(hs_type[ll+len('CList '):])))

    return cls

def argtype(hs_type):
    '''
    returns: tuple : (type of argument, constructor)
    '''
    ll = hs_type.find('CList ')
    argtype = hs2py(hs_type)
    if ll < 0:
        arr = hs_type.find('CArray ')
        if arr < 0:
            return argtype, argtype
        else:
            return argtype, partial(to_c_array, argtype._type_)
    else:
        return argtype, partial(to_linked_list, argtype._type_)

def restype(hs_type):
    '''
    returns: tuple : (type of result, reconstructor, bool: needsFinalizer)
    '''
    ll = hs_type.find('CList ')
    restype = hs2py(hs_type)
    if ll < 0:
        arr = hs_type.find('CArray ')
        if arr < 0:
            return restype, lambda x:x, False
        else:
            return restype, from_c_array, True
    else:
        return restype, from_linked_list, True

def strip_io(tp):
    '''
    IO is somewhat disregarded in the FFI exports. IO CDouble
    looks the same as CDouble from Python's side. So we remove
    the monadic part from our type to process the rest.
    '''
    io = tp.find('IO ')
    if io < 0:
        return tp
    else:
        return tp[io+3:]

def hs_type_to_py(hs_type):
    *_,hs_type = hs_type.split('=>')
    *inp,out = map(lambda x: x.strip(),hs_type.split('->'))
    out = strip_io(out)
    
    return (
        [argtype(i) for i in inp],
        restype(out)
        )