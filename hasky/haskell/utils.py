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