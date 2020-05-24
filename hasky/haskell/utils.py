from .types import hs_type_to_py

def remove_trailing_comment(hs_line):
    if '--' in hs_line:
        return hs_line.split('--')[0].strip()
    else:
        return hs_line

def process_hs_lines(hs_line, f):
    hs_line = remove_trailing_comment(hs_line)
    in_comment = False
    for hs_line in hs_line.split(';'):
        in_comment = '{-' in hs_line
        if in_comment:
            in_comment = not '-}' in hs_line
        if in_comment or hs_line.startswith('--'):
            continue
        f(hs_line)

def get_exported(hs_file):
    return dict(_get_exported(hs_file))

def _get_exported(hs_file):
    with open(hs_file, 'r') as f:
        for line in f.readlines():
            yield from process_hs_lines(line, _exported)

def _exported(hs_line):
    if hs_line.startswith('foreign export ccall'):
        func_export,type = hs_line.split('::')
        *_,name = func_export.strip().split(' ')
        yield name, hs_type_to_py(type)