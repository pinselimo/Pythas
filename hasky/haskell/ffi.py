import os.path

from .parse_file import parse_haskell

FFI_INDICATOR = '_hasky_ffi'

def create_ffi_file(filename, parse_info):
    res = []
    if parse_info.exported_ffi:
        res.append((filename, parse_info))
    if parse_info.exported_mod:
        file_name = ffi_filename(parse_info)
        file_content = _create_ffi_file(parse_info)
        with open(file_name, 'w') as f:
            f.write('\n'.join(file_content))
        res.append((file_name, parse_haskell(file_name)))
    return res

def ffi_filename(parse_info):
    module_name = parse_info.name + FFI_INDICATOR
    return os.path.join(parse_info.dir,module_name+'.hs')

def _create_ffi_file(parse_info):
    '''
    Creates a .hs module <module_name> as <ffi_filename> holding the foreign exports 
    for the exported functions of module <import_name>.
    '''
    LINE_SEP = ''
    LANGUAGE_FFI = "{-# LANGUAGE ForeignFunctionInterface #-}"
    MODULE_DEF = "module {} where".format(parse_info.name + FFI_INDICATOR)
    IMPORT_FOREIGN = (
        "import Foreign.Ptr",
        "import Foreign.Storable",
        "import Foreign.C.Types",
        "import Foreign.C.String",
        "import HaskyList",
        "import HaskyArray",
    )
    
    IMPORT_HS = "import qualified {} ({})".format(parse_info.name, ', '.join(parse_info.exported_mod))
    FOREIGN_EXPORT = "foreign export ccall {} :: {}"
    INTERNAL_DEF = "{} = {}.{}"
    SIMPLE_DEF = "{} = {}"
    DESTRUCTOR_TYPE = "{} -> IO ()"

    file = [
        LANGUAGE_FFI,
        MODULE_DEF,
        LINE_SEP,
        *IMPORT_FOREIGN,
        LINE_SEP,
        IMPORT_HS,
        LINE_SEP,
    ]

    for n in parse_info.exported_mod:
        func_info = parse_info.func_infos[n]
        file.append(
            FOREIGN_EXPORT.format(n, func_info.exporttype)
        )
        if func_info.exporttype == func_info.functype:    
            file.append(
                INTERNAL_DEF.format(n,parse_info.name,n)
            )
        else:
            file.append(
                construct_func(parse_info.name,func_info)
            )
        if func_info.destr_type:
            dest_name = n + 'Finalizer'
            destr_type = DESTRUCTOR_TYPE.format(func_info.destr_type)
            file.append(
                FOREIGN_EXPORT.format(dest_name, destr_type)
            )
            file.append(
                SIMPLE_DEF.format(dest_name, func_info.destructor)
            )

    return file

def construct_func(ori_mod_name, func_info):
    n = func_info.name
    qualified_func = "{}.{}".format(ori_mod_name,n)
    argc = len(func_info.argtypes)
    args = [a[0] for a in zip("abcdefghijklmnop",range(argc))]
    arg_convert = ['({} {})'.format(c,a) for a,c in zip(args, func_info.native_from_c)]

    args = ' '.join(args)
    arg_convert = ' '.join(arg_convert)
    back = func_info.native_to_c
    if not back:
        back = 'id'
    return "{} {} = {} $ {} {}".format(n, args, back, qualified_func, arg_convert)