"""Custom types defined for Haskell FFI interfacing of complex, nested data."""

import ctypes as cl
from functools import partial


def get_constructor(ctype):
    """Finds the constructor for a standard or custom ctypes type.
    The custom types are checked against the marker classes:

    - Array
    - Tuple
    - LinkedList

    Parameters
    ----------
    ctype : ctypes type
        (Sub-)Class of ctypes._SimpleCData or ctypes.Structure.

    Returns
    -------
    constructor : callable
        Function creating an instance of `ctype`.
    """
    if issubclass(ctype, cl._Pointer):
        subtype = ctype._type_

        if issubclass(subtype, Array):
            constr = lambda x: cl.pointer(to_c_array(subtype, x))

        elif issubclass(subtype, Tuple):
            constr = lambda x: cl.pointer(to_tuple(subtype, x))

        elif issubclass(subtype, LinkedList):
            constr = lambda x: cl.pointer(to_linked_list(subtype, x))

        else:
            # For any pointer the value needs to be
            # packed first in the subtype and then
            # in the actual type
            constr = lambda x: ctype(subtype(x))

    else:
        constr = ctype

    return constr


class LinkedList:
    """Marker class for Pythas' linked lists"""

    pass


def new_linked_list(ctype):
    """Creates a Pythas linked list class of `ctype`.

    Parameters
    ----------
    ctype : ctypes type
        (Sub-)Class of `ctypes._SimpleCData` or `ctypes.Structure`.

    Returns
    -------
    c_linked_list : ctypes type
        Subclass of `LinkedList` and `ctypes.Structure`.
    """

    class c_linked_list(LinkedList, cl.Structure):
        pass

    c_linked_list._fields_ = [("value", ctype), ("next", cl.POINTER(c_linked_list))]
    return c_linked_list


def to_linked_list(cls, seq):
    """Constructor function for Pythas linked lists.

    Any instance of LinkedList must always be associated
    with its `cls`.

    Parameters
    ----------
    cls : LinkedList subclass
        A linked list class created with `new_linked_list`.
    seq : Sequence
        The sequence which needs to be converted to a linked list.

    Returns
    -------
    linked_list : LinkedList
        An instance of `cls` constructed from `seq`.

    See Also
    --------
    new_linked_list
    """
    constructor = cls._fields_[0][1]
    *rest, last = map(constructor, seq)

    lel = cls()
    lel.value = last
    lel.next = cl.POINTER(cls)()  # nullPtr

    for elem in rest[::-1]:
        prev = cls()
        prev.value = elem
        prev.next = cl.pointer(lel)

        lel = prev

    return cl.pointer(lel)


def from_linked_list(ll):
    """Reconstructor from Pythas linked lists.

    Parameters
    ----------
    ll : LinkedList
        Pointer to the first element of a LinkedList instance.

    Returns
    -------
    seq : list
        A list with the contents of `ll`.
    """
    val = ll.contents.value
    next = ll.contents.next

    seq = [val]
    while bool(next):
        val = next.contents.value
        seq.append(val)

        next = next.contents.next

    return seq


class Array:
    """Marker class for Pythas' array types"""

    pass


def new_c_array(ctype):
    """Creates a Pythas array class of `ctype`.

    Parameters
    ----------
    ctype : ctypes type
        (Sub-)Class of `ctypes._SimpleCData` or `ctypes.Structure`.

    Returns
    -------
    c_array : ctypes type
        Subclass of `Array` and `ctypes.Structure`.
    """

    class c_array(Array, cl.Structure):
        _fields_ = [("len", cl.c_int), ("ptr", cl.POINTER(ctype))]

    return c_array


def to_c_array(cls, seq):
    """Constructor function for Pythas array.

    Any instance of Array  must always be associated with its `cls`.

    Parameters
    ----------
    cls : Array subclass
        A array class created with `new_c_array`.
    seq : Sequence
        The sequence which needs to be converted to a c_array.

    Returns
    -------
    array : Array
        An instance of `cls` constructed from `seq`.

    See Also
    --------
    new_c_array
    """
    ctype = cls._fields_[1][1]._type_

    content = map(get_constructor(ctype), seq)
    array = (ctype * len(seq))(*content)

    arr = cls()
    arr.len = cl.c_int(len(seq))
    arr.ptr = cl.cast(array, cl.POINTER(ctype))
    return arr


def from_c_array(cp_array):
    """Reconstructor from Pythas c_arrays.

    Parameters
    ----------
    cp_array : cl.POINTER(Array)
        Pointer to an instance of a subclass of Array.

    Returns
    -------
    seq : list
        A list with the contents of `cp_array`'s array.
    """
    c_arr = cp_array.contents
    return [c_arr.ptr[i] for i in range(c_arr.len)]


class Tuple:
    """Marker class for Pythas' tuples"""

    pass


def new_tuple(subtypes):
    """Creates a constructor for a Pythas tuple from Python tuples of `subtypes`.

    Parameters
    ----------
    subtypes : Sequence of ctypes types
        (Sub-)Class of `ctypes._SimpleCData` or `ctypes.Structure`.

    Returns
    -------
    c_tuple : ctypes type
        Subclass of `Tuple` and `ctypes.Structure`.
    """

    class c_tuple(Tuple, cl.Structure):
        _fields_ = list(zip("abcd", subtypes))

    return c_tuple


def to_tuple(cls, tup):
    """Constructor function for Pythas tuples.

    Any instance of LinkedList must always be associated with its `cls`.

    Parameters
    ----------
    cls : Tuple subclass
        A tuple class created with `new_tuple`.
    tup : tuple
        The tuple which needs to be converted to a Pythas Tuple.

    Returns
    -------
    tuple : Tuple
        An instance of `cls` constructed from `tup`.

    See Also
    --------
    new_tuple
    """
    types = [cls._fields_[n][1] for n in range(len(cls._fields_))]
    return cls(*[get_constructor(t)(v) for t, v in zip(types, tup)])


def from_tuple(cpt):
    """Reconstructor from Pythas c_tuples.

    Parameters
    ----------
    cpt : cl.POINTER(Tuple)
        Pointer to an instance of a subclass of Tuple.

    Returns
    -------
    tup : tuple
        A tuple with the contents of `cpt`.
    """
    ct = cpt.contents
    return tuple(getattr(ct, a) for a in "abcd" if hasattr(ct, a))
