import os.path

from .utils import get_from_hsfile, TAG_EXCLUDE

def _get_type_decl(hs_line):
    if '::' in hs_line:
        yield hs_line

def _get_excluded_name(hs_line):
    if hs_line.startswith(TAG_EXCLUDE):
        yield hs_line.split(TAG_EXCLUDE)[-1].strip()

get_type_decls = lambda hs_file: get_from_hsfile(hs_file, _get_type_decl)
get_excluded_names = lambda hs_file: get_from_hsfile(hs_file, _get_excluded_name)

def get_functions_to_export(type_decls, excluded_names):
    type_decls = tuple(type_decls)
    foreigns = {
        t.split(' ')[3]
        for t in type_decls
        if t.startswith('foreign')
        }
    foreigns.add('foreign')
    return filter(
        lambda n: not (n[0] in foreigns or n[0] in excluded_names), 
        ((t.split(' ')[0],t.split('::')[-1].strip()) for t in type_decls)
        )

def create_ffi_file(import_name, module_name, ffi_filename, expose_name_types_tuples):
    ntts = tuple(expose_name_types_tuples)
    LINE_SEP = ''
    LANGUAGE_FFI = "{-# LANGUAGE ForeignFunctionInterface #-}"
    MODULE_DEF = "module {} where".format(module_name)
    IMPORT_FOREIGN = (
        "import Foreign",
        "import Foreign.Ptr",
        "import Foreign.Storable",
        "import Foreign.C.Types",
        "import Foreign.C.String"
    )
    IMPORT_HS = "import qualified {} ({})".format(import_name, ', '.join(n[0] for n in ntts))
    FOREIGN_EXPORT = "foreign export ccall {} :: {}"
    INTERNAL_DEF = "{} = {}.{}"

    file = [
        LANGUAGE_FFI,
        MODULE_DEF,
        LINE_SEP,
        *IMPORT_FOREIGN,
        LINE_SEP,
        IMPORT_HS,
        LINE_SEP,
    ]

    for n,t in ntts:
        file.append(
            FOREIGN_EXPORT.format(n,t)
        )
        file.append(
            INTERNAL_DEF.format(n,import_name,n)
        )

    with open(ffi_filename, 'w') as f:
        f.write('\n'.join(file))

def create_ffi(hs_file):
    ffi_files = []

    type_decls = tuple(get_type_decls(hs_file))
    excluded_names = set(get_excluded_names(hs_file))
    name_type_tuples = tuple(get_functions_to_export(type_decls, excluded_names))

    if len(name_type_tuples) < len(type_decls): # foreign stuff declared in hs_file
        ffi_files.append(hs_file)

    if len(name_type_tuples) > 0: # some functions are not foreign in hs_file
        *path,file = os.path.split(hs_file)
        import_name = file.split('.')[0]
        module_name = import_name + '_hasky_ffi'
        ffi_filename = os.path.join(*path, module_name + '.hs')

        create_ffi_file(import_name, module_name, ffi_filename, name_type_tuples)
        ffi_files.append(ffi_filename)
    
    return ffi_files