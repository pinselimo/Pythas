import os
import sys
from ctypes import _SimpleCData, _Pointer
from collections import abc
from shutil import which

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
    def __init__(self, name, func_info, funcPtr, destructorPtr):
        self.__name__ = name
        self._funcPtr = funcPtr
        self.argtypes = func_info.argtypes
        self.constructors = func_info.constructors
        self.reconstructor = func_info.reconstructor
        self.restype = func_info.restype
        self.destructor = destructorPtr

        self._funcPtr.argtypes = list(func_info.argtypes)
        self._funcPtr.restype = func_info.restype

    def __call__(self, *args):
        args = flatten([constr(a) for constr,a in zip(self.constructors, args)])
        retVal = self._funcPtr(*args)
        res = self.reconstructor(retVal)
        if self.destructor:
            self.destructor(retVal)
        return res

def find_source(name, path, extension='.hs', transform=lambda s:s.capitalize()):
    hsName = transform(name) + extension
    for file in os.listdir(path):
        if file == hsName:
            return [os.path.join(path,file)]
    else:
        return []

def custom_attr_getter(obj, name):
    ffi_libs = obj.ffi_libs
    not_found = AttributeError("{} object has no attribute {} and no Haskell module containing it.".format(obj.__name__,repr(name)))
    for lib, info in ffi_libs:
        if name in info.exported_ffi:
            f = getattr(lib,name)
            func_infos = info.func_infos[name]
            if is_constant(func_infos):
                return f()
            if func_infos.destroy:
                destrPtr = getattr(lib,name + 'Finalizer')
            else:
                destrPtr = None
            return HaskyFunc(name, func_infos, f, destrPtr)
    else:
        raise not_found

def check_ctype_seq(seq):
    def _check(seq):
        return any(not isinstance(e, _SimpleCData) if not isinstance(e,abc.Iterable) else _check(e) for e in seq)

    if not _check(seq):
        raise TypeError('Only sequences of <ctypes._SimpleCData allowed.')
    else:
        return seq

def is_constant(func_infos):
    return not (func_infos.argtypes or 'IO' in func_infos.htype)

def check_has_ghc():
    if not (which('ghc') or which('stack')):
        raise ImportError('No GHC found. Please install either Stack or GHC and make sure that either is in you $PATH.')

