import os
import sys

def findhs(name, path):
    hsName = name.capitalize() + ".hs"
    for root,_,files in os.walk(path):
        for file in files:
            if file == hsName:
                return [os.path.join(root,file)]
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
    
def is_external_library(pack):
    if "hasky" in pack:
        return False
    else:
        return pack[-1] in sys.modules.keys()