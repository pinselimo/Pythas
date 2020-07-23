import ctypes as cl
from functools import partial

class LinkedList:
    pass

def new_linked_list(ctype):
    class c_linked_list(LinkedList, cl.Structure):
        pass
    c_linked_list._fields_ = [('value',ctype),('next',cl.POINTER(c_linked_list))]
    return c_linked_list

def to_linked_list(cls, seq):
    valConstr = cls._fields_[0][1]
    *rest,last = map(valConstr,seq)
    lel = cls()
    lel.value = last
    lel.next = cl.POINTER(cls)() # nullPtr
    for elem in rest[::-1]:
        prev = cls()
        prev.value = elem
        prev.next = cl.pointer(lel)
        lel = prev
    return cl.pointer(lel)

def from_linked_list(ll):
    val = ll.contents.value
    next = ll.contents.next
    res = [val]
    while bool(next):
        val = next.contents.value
        res.append(val)
        next = next.contents.next
    return res

class Array:
    pass

def new_c_array(ctype):
    class c_array(Array, cl.Structure):
        _fields_ = [('len',cl.c_int),('ptr',cl.POINTER(ctype))]
    return c_array

def to_c_array(cls, seq):
    arr = cls()
    arr.len = cl.c_int(len(seq))
    ctype = cls._fields_[1][1]._type_
    if issubclass(ctype, cl._Pointer):
        subtype = ctype._type_
        if issubclass(subtype, Array):
            subconstr = lambda x: cl.pointer(to_c_array(subtype,x))
            content = map(subconstr,seq)
        elif issubclass(subtype, LinkedList):
            subconstr = lambda x: cl.pointer(to_linked_list(subtype,x))
            content = map(subconstr, seq)
        else:
            # For any pointer the value needs to be 
            # packed first in the subtype and then
            # in the actual type
            subconstr = lambda x: ctype(subtype(x))
            content = map(subconstr,seq)
    else:
        content = map(ctype,seq)
    a = (ctype * len(seq))(*content)
    arr.ptr = cl.cast(a,cl.POINTER(ctype))
    return arr

def from_c_array(cp_array):
    c_arr = cp_array.contents
    return [c_arr.ptr[i] for i in range(c_arr.len)]

class Tuple2:
    pass

def new_tuple2(atype, btype):
    class c_tuple2(Tuple2, cl.Structure):
        _fields_ = [('a',atype),('b',btype)]
    return c_tuple2

def from_tuple2(cpt):
    ct = cpt.contents
    return (ct.a, ct.b)

class Tuple3:
    pass

def new_tuple3(atype, btype, ctype):
    class c_tuple3(Tuple3, cl.Structure):
        _fields_ = [('a',atype),('b',btype),('c',ctype)]
    return c_tuple3

def from_tuple3(cpt):
    ct = cpt.contents
    return (ct.a, ct.b, ct.c)

class Tuple4:
    pass

def new_tuple4(atype, btype, ctype, dtype):
    class c_tuple4(Tuple4, cl.Structure):
        _fields_ = [('a',atype),('b',btype),('c',ctype),('d',dtype)]
    return c_tuple4

def from_tuple4(cpt):
    ct = cpt.contents
    return (ct.a, ct.b, ct.c, ct.d)

