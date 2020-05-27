import ctypes as cl
from collections.abc import Iterable

def check_ctype_seq(seq):
    def _check(seq):
        return any(not isinstance(e, cl._SimpleCData) if not isinstance(e,Iterable) else _check(e) for e in seq)

    if not _check(seq):
        raise TypeError('Only sequences of <ctypes._SimpleCData allowed.')
    else:
        return seq

def new_linked_list(ctype):
    class c_linked_list(cl.Structure):
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

def to_c_array(cls, seq):
    seq = check_ctype_seq(seq)
    return (cls * len(seq))(*seq), len(seq)

def from_c_array(cp_array, length):
    return [cp_array[i] for i in range(length)]
