import os
import sys

DOT = '.'

def findSource(name, path, extension='.hs', transform=lambda s:s.capitalize()):
    hsName = transform(name) + extension
    print(hsName)
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
            argtypes, restype = funcs[name]
            f.argtypes = argtypes
            f.restype = restype
            return f
    else:
        raise not_found