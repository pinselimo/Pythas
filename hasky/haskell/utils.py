def applyT2(fs,t):
    fa,fb = fs
    x,y = t
    return (fa[1](x), fb[1](y))

def applyT3(fs,t):
    fa,fb,fc = fs
    x,y,z = t
    return (fa[1](x), fb[1](y), fc[1](z))

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

def tuple_types(hs_type):
    '''
    Nested tuples cannot just be .split(') ('), the
    inner tuples have to be preserved for further processing.
    '''
    subtuples = [hs_type.find('CTuple')]
    while subtuples[-1]+1:
        subtuples.append(hs_type.find('CTuple',subtuples[-1]+1))
    if len(subtuples) == 1:
        return hs_type.split(') (')
    else:
        splits = [hs_type.find(') (',i)+1 if i != len(hs_type) else i
                for i in map(lambda i: match_parens(hs_type,i-1), subtuples) if i
            ]
        res = [hs_type[a:b] for a,b in zip([0]+splits,splits+[len(hs_type)])]
        return res

def match_parens(s, i):
    '''
    Given a string and the index of the opening
    parenthesis returns the index of the closing one.
    '''
    if i < 0:
        return 0
    x = 0
    for it in range(i,len(s)):
        c = s[it]
        if c == '(':
            x += 1
        elif c == ')':
            x -= 1
        if x == 0:
            return it
    else:
        return len(s)

