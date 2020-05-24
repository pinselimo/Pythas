import ctypes as cl

class TypeConverter:
    @property
    def HS2PY(self):
        '''
        HS2PY maps which Haskell type will end up as which
        ctypes type at Python's side
        '''
        return {
            ### void ###
            '()':None,

            ### INTEGRAL ####
            'CInt':cl.c_int32,
            'Int':cl.c_int32, # fascinatingly works so far
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

            ### Arraytypes ###
            '__TODO__':pyCArr
        }

    @property
    def PY2HS(self):
        '''
        PY2HS defined which Haskell input type
        requires which Haskell side type conversion
        '''
        return dict()

def pyCArr(data):
    if any(not isinstance(elem, cl._SimpleCData) for elem in data):
        raise TypeError('Only sequences of <ctypes._SimpleCData allowed.')
    if len(data) > 0:
        array_type = data[0]
        if not all(isinstance(elem,type(), array_type) for elem in data):
            raise TypeError('Multiple types encountered in sequence')
        return (array_type * len(data))(*data)

def strip_io(tp):
    '''
    IO is somewhat disregarded in the FFI exports. IO CDouble
    looks the same as CDouble from Python's side. So we remove
    the monadic part from our type to process the rest.
    '''
    if tp.startswith('IO'):
        return tp[3:]
    else:
        return tp

def hs_type_to_py(hs_type):
    *_,hs_type = hs_type.split('=>')
    *inp,out = map(lambda x: x.strip(),hs_type.split('->'))
    out = strip_io(out)
    tc = TypeConverter()
    
    return ( # c_int is the standard set by ctypes.cdll
        [tc.HS2PY[i] if i in tc.HS2PY else cl.c_int for i in inp], # argtypes
        tc.HS2PY[out] if out in tc.HS2PY else cl.c_int # restype
        )