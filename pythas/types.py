import ctypes as cl
from functools import partial

def get_constructor(ctype):
    if issubclass(ctype, cl._Pointer):
        subtype = ctype._type_
        if issubclass(subtype, Array):
            constr = lambda x: cl.pointer(to_c_array(subtype,x))
        elif issubclass(subtype, Tuple):
            constr = lambda x: cl.pointer(to_tuple(subtype,x))
        elif issubclass(subtype, LinkedList):
            constr = lambda x: cl.pointer(to_linked_list(subtype,x))
        else:
            # For any pointer the value needs to be
            # packed first in the subtype and then
            # in the actual type
            constr = lambda x: ctype(subtype(x))
    else:
        constr = ctype
    return constr

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
    content = map(get_constructor(ctype), seq)
    a = (ctype * len(seq))(*content)
    arr.ptr = cl.cast(a,cl.POINTER(ctype))
    return arr

def from_c_array(cp_array):
    c_arr = cp_array.contents
    return [c_arr.ptr[i] for i in range(c_arr.len)]

class Tuple:
    pass

def new_tuple(subtypes):
    class c_tuple(Tuple, cl.Structure):
        _fields_ = list(zip("abcd",subtypes))
    return c_tuple

def to_tuple(cls, tup):
    types = [cls._fields_[n][1] for n in range(len(cls._fields_))]
    return cls(*[get_constructor(t)(v) for t,v in zip(types,tup)])

def from_tuple(cpt):
    ct = cpt.contents
    return tuple(getattr(ct,a) for a in "abcd" if hasattr(ct, a))

