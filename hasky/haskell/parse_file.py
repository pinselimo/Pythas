import os.path

from ..parser import ParseInfo, FuncInfo
from .parse_type import parse_type

def parse_haskell(hs_file):
    # preprocessing of file
    *path, name = os.path.split(hs_file)
    filedir = os.path.join(*path)
    name = name[:name.find('.hs')]
    with open(hs_file, 'r') as f:
        contents = f.readlines()

    parse_info = ParseInfo(name, filedir, set(), set(), dict())
    parse_info = _parse_haskell(contents, parse_info)

    exported_mod = parse_head(contents, name)
    if exported_mod is None:
        parse_info.exported_mod.update(
            set(parse_info.func_infos.keys()) - parse_info.exported_ffi
        )
    else:
        parse_info.exported_mod.update( exported_mod )

    return parse_info

def _parse_haskell(hs_lines, parse_info):
    in_comment = False
    for hs_line in hs_lines:
        for hs_line in hs_line.split(';'):
            # Pre-processing of hs_line
            hs_line = hs_line.strip()
            in_comment = '{-' in hs_line
            if not (in_comment or hs_line.startswith('\n') or hs_line.startswith('--')):
                parse_line(hs_line, parse_info)
            elif in_comment:
                in_comment = not '-}' in hs_line
    else:
        return parse_info

def find_module_statement(hs_cont, name):
    module_name = 'module {}'.format(name)
    module_decl = hs_cont.find(module_name)
    if module_decl > -1:
        return module_decl + len(module_name)
    else:
        raise SyntaxError('Haskell file module statement malformed (Case sensitive!)')

def parse_head(hs_lines, name):
    '''
    Finds all the names that are exported according to the module statement.

    Returns None if there is no Statement.
    Returns an empty list if no names are exported.
    Returns a list of names exported.
    '''
    hs_cont = ' '.join(hs_lines)
    name,*_ = name.split('.')
    module_decl_end = find_module_statement(hs_cont, name)
    head = hs_cont[module_decl_end:hs_cont.find('where')].strip()
    if len(head) == 0:
        return None
    else:
        head = head.strip('() \n')
    if len(head) == 0:
        return set()
    else:
        return {n.strip() for n in head.split(',')}

def parse_line(hs_line, parse_info):
    if hs_line.startswith('foreign export ccall'):
        func_export,type_def = hs_line.split('::')
        *_,name = func_export.strip().split(' ')
        name = name.strip()
        parse_info.exported_ffi.add(name)
        parse_info.func_infos[name] = parse_type(name, type_def)

