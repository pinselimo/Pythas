import os
import sys
from ctypes import _SimpleCData, _Pointer
from collections import abc

DOT = '.'

fst = lambda x:x[0]
snd = lambda x:x[1]
thd = lambda x:x[2]

def flatten(seq):
    def flat(ts):
        if isinstance(ts, abc.Iterable):
            for t in ts:
                yield from flat(t)
        else:
            yield ts
    return list(flat(seq))

class HaskyFunc:
    def __init__(self, name, funcPtr, argtypes, constructors, restype, reconstructor, destructor=None):
        self.__name__ = name
        self._funcPtr = funcPtr
        self.argtypes = argtypes
        self.constructors = constructors
        self.reconstructor = reconstructor
        self.restype = restype
        self.destructor = destructor

        self._funcPtr.argtypes = list(argtypes)
        self._funcPtr.restype = restype

    def __call__(self, *args):
        args = flatten([constr(a) for constr,a in zip(self.constructors, args)])
        retVal = self._funcPtr(*args)
        res = self.reconstructor(retVal)
        if self.destructor:
            self.destructor(retVal)
        return res

def findSource(name, path, extension='.hs', transform=lambda s:s.capitalize()):
    hsName = transform(name) + extension
    for file in os.listdir(path):
        if file == hsName:
            return [os.path.join(path,file)]
    else:
        return []

def custom_attr_getter(obj, name):
    ffi_libs = obj.ffi_libs
    not_found = AttributeError("{} object has no attribute {} and no Haskell module containing it.".format(obj.__name__,repr(name)))
    for lib, funcs in ffi_libs:
        if name in funcs:
            f = getattr(lib,name)
            argTuples, resTuple = funcs[name]
            needs_finalizer = thd(resTuple)
            
            if needs_finalizer:
                finalizer = getattr(lib,name+'Finalizer')
            
            return HaskyFunc(name, f, map(fst,argTuples), map(snd,argTuples), fst(resTuple), snd(resTuple), finalizer)
    else:
        raise not_found