import ctypes as cl
from functools import partial

from ..types import *
from ..parser import FuncInfo
from .utils import applyT2, applyT3, strip_io

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
    ll = hs_type.find('CList ')
    arr = hs_type.find('CArray ')
    t2 = hs_type.find('CTuple2 ')
    t3 = hs_type.find('CTuple3 ')
    ## Linked List first
    if ll+1 and (ll < arr or arr < 0) and (ll < t2 or t2 < 0) and (ll < t3 or t3 < 0):
        cls = cl.POINTER(new_linked_list(hs2py(hs_type[ll+len('CList '):])))
    ## Array first
    elif arr+1 and (arr < t2 or t2 < 0) and (arr < t3 or t3 < 0):
        cls = cl.POINTER(new_c_array(hs2py(hs_type[arr+len('CArray '):])))
    ## Tuple of 2 first
    elif t2+1 and (t2 < t3 or t3 < 0):
        hs_type = hs_type[t2+len('CTuple2 '):]
        hs_type_a, hs_type_b = tuple_types(hs_type)
        cls = cl.POINTER(new_tuple2(hs2py(hs_type_a), hs2py(hs_type_b)))
    ## Tuple of 3 first
    elif t3+1:
        hs_type = hs_type[t3+len('CTuple3 '):]
        hs_type_a, hs_type_b, hs_type_c = tuple_types(hs_type)
        cls = cl.POINTER(new_tuple3(hs2py(hs_type_a), hs2py(hs_type_b), hs2py(hs_type_c)))
    ## Non-packed types
    else:
        cls = simple_hs_2_py(hs_type)
    return cls

def argtype(hs_type):
    '''
    returns: tuple : (type of argument, constructor)
    '''
    argt = hs2py(hs_type)
    ll = hs_type.find('CList ')
    arr = hs_type.find('CArray ')
    st = hs_type.find('CWString')
    if ll+1 and (ll < arr or arr < 0): ## Linked List first
        # subconstr = argtype(hs_type[ll+len('CList '):])[1]
        constr = lambda x: partial(to_linked_list, argt._type_)(list(map(subconstr,x)))
    elif arr+1 and (arr < ll or ll < 0): ## array first
        # subconstr = argtype(hs_type[arr+len('CArray '):])[1]
        # constr = lambda x: partial(to_c_array, argt._type_)(list(map(subconstr,x)))
        constr = partial(to_c_array, argt._type_)
    elif st+1:
        constr = lambda x: cl.pointer(cl.c_wchar_p(x))
    else: # neither linked list nor array
        constr = argt
    return argt, constr

def restype(hs_type):
    '''
    returns: tuple : (type of result, reconstructor, bool: needsFinalizer)
    '''
    rtype = hs2py(hs_type)
    final = True
    ll = hs_type.find('CList ')
    arr = hs_type.find('CArray ')
    t2 = hs_type.find('CTuple2 ')
    t3 = hs_type.find('CTuple3 ')
    st = hs_type.find('CWString')
    ## Linked List first
    if ll+1 and (ll < arr or arr < 0) and (ll < t2 or t2 < 0) and (ll < t3 or t3 < 0):
        inner = restype(hs_type[ll+len('CList '):])[1]
        recon = lambda x: list(map(inner,from_linked_list(x)))
    ## Array first
    elif arr+1 and (arr < t2 or t2 < 0) and (arr < t3 or t3 < 0):
        inner = restype(hs_type[arr+len('CArray '):])[1]
        recon = lambda x: list(map(inner,from_c_array(x)))
    ## Tuple of 2 first
    elif t2+1 and (t2 < t3 or t3 < 0):
        hs_type = hs_type[t2+len('CTuple2 '):]
        hs_inner = tuple_types(hs_type)
        inner = map(restype,hs_inner)
        recon = lambda x: applyT2(inner, from_tuple2(x))
    ## Tuple of 3 first
    elif t3+1:
        hs_type = hs_type[t3+len('CTuple3 '):]
        hs_inner = tuple_types(hs_type)
        inner = map(restype,hs_inner)
        recon = lambda x: applyT3(inner, from_tuple3(x))
    elif st+1:
        recon = lambda x:x.contents.value
    else:
        recon = lambda x:x
        final = False
    return rtype, recon, final

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

    restp, reconstructor, destroy = restype(out)

    return FuncInfo(
              name, argtypes, restp, constructors
            , reconstructor, destroy, hs_type
            )

