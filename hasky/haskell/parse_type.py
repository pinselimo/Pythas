import ctypes as cl
from functools import partial

from ..types import *
from ..parser import FuncInfo

USE_LIST = True

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
    }

HS2HS = {
    'Int':('CInt','fromIntegral','fromIntegral'),
    'Integer':('CLLong','fromIntegral','fromIntegral'),
    'Bool':('CBool','fromBool','toBool'),
}

def hs_2_hsc(hs_type):
    hs_type = hs_type.strip('( )')
    if hs_type == "":
        return '()', '', ''

    if hs_type in HS2PY or 'CList' in hs_type or 'CArray' in hs_type:
        return hs_type, '', ''

    elif hs_type in HS2HS:
        return HS2HS[hs_type]

    elif hs_type.startswith('[') and hs_type.endswith(']'):
        inner_type, from_c, to_c = hs_2_hsc(hs_type[1:-1])
        if USE_LIST:
            if from_c:
                from_c = 'newList $ map {}'.format(from_c)
            else:
                from_c = 'newList'
            if to_c:
                to_c = '(map {}) $ fromList'.format(to_c)
            else:
                to_c = 'fromList'

            hsc_type = '(CList {})'.format(inner_type)
        else: # use array
            if from_c:
                from_c = 'newArray $ map {}'.format(from_c)
            else:
                from_c = 'newArray'
            if to_c:
                to_c = '(map {}) $ fromArray'.format(to_c)
            else:
                to_c = 'fromArray'

            hsc_type = '(CArray {})'.format(inner_type)

        
        return hsc_type, from_c, to_c

    elif hs_type.islower():
        raise TypeError('Typevariables cannot be used with the FFI')
    else:
        raise TypeError('Non-simple type "{}" cannot be used with Hasky'.format(hs_type))

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
            return restype, lambda x:x, None, None
        else:
            return restype, from_c_array, 'freeArray', hs_type
    else:
        return restype, from_linked_list, 'freeList', hs_type

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

def reconstruct_hs_type(constraints, inp, io, out):
    t = ''
    if constraints:
        t += "({}) => ".format(constraints)
    if inp:
        t +=  ' -> '.join(inp) + ' -> '
    if out == '()' and io:
        t += 'IO ()'
    elif io or 'CList' in out or 'CArray' in out:
        t += 'IO ({})'.format(out)
    else:
        t += '{}'.format(out)
    return t
        
def parse_type(name, hs_type):
    *constraints,hs_type = hs_type.split('=>')
    types = [t.strip() for t in hs_type.split('->')]
    if any(('(' in t) != (')' in t) for t in types):
        raise TypeError('Functions as arguments like "{}" not supported'.format(hs_type))

    *inp,out = types
    io, out = strip_io(out)

    rev_inp = list()
    
    native_from_c = []
    native_to_c = ''
    for i in inp:
        hsc_type, to_c, from_c = hs_2_hsc(i)
        native_from_c.append(from_c)
        rev_inp.append(hsc_type)
    else:
        rev_out, to_c, from_c = hs_2_hsc(out)
        native_to_c = to_c
   
    argtypes = list()
    constructors = list()
    for i in rev_inp:
        argt, constructor = argtype(i)
        argtypes.append(argt)
        constructors.append(constructor)

    restp, reconstructor, destructor, destr_type = restype(rev_out)

    functype = reconstruct_hs_type(constraints, inp, io, out)
    exporttype = reconstruct_hs_type(constraints, rev_inp, io, rev_out)
    return FuncInfo(
        name, functype, exporttype, argtypes, restp, constructors, 
        reconstructor, destructor, destr_type, native_from_c, native_to_c
        )