import subprocess
import re
import os
import sys

from .haskell.types import hs_type_to_py

REGEX_HS_VERSION = b'(?<=[a-z A-Z])[0-9.]{5}'
REGEX_C_CONSTANTS = '#define[ \t\n\r\f\v]+([a-zA-Z0-9_]+)[ \t\n\r\f\v]+([0-9+])' # This causes warning: '#define\s+(\w+)\s+([0-9]+)'

GHC_VERSION_H = '/usr/lib/ghc/include/ghcversion.h'

__GLASGOW_HASKELL__ = "__GLASGOW_HASKELL__"
__GLASGOW_HASKELL_PATCHLEVEL1__ = "__GLASGOW_HASKELL_PATCHLEVEL1__"

class GHC_Exception(Exception):
    pass

def get_ghc_version_from_cmdln():
    stdout = subprocess.run(["ghc","--version"],capture_output=True).stdout
    version = re.search(REGEX_HS_VERSION, stdout )
    return version.group(0).decode("utf-8")
        
def get_ghc_version_from_header():
    consts = {}
    with open(GHC_VERSION_H,'r') as header:
        for name, value in re.findall(REGEX_C_CONSTANTS, header.read()):
            consts[name] = value
            try:
                pl = consts[__GLASGOW_HASKELL_PATCHLEVEL1__]
            except KeyError:
                pl = '0'
            try:
                vn = consts[__GLASGOW_HASKELL__]
                v = str(int(vn)//100)
                n = str(int(vn)%100)
                return v+'.'+n+'.'+pl
            except KeyError:
                raise GHC_Exception("Version-Number of GHC could not be found")

def get_ghc_version():
    try:
        return get_ghc_version_from_cmdln()
    except AttributeError: # Regex didn't match, fallback
        return get_ghc_version_from_header()

GHC_VERSION = get_ghc_version()

def findhs(name, path):
    hsName = name.capitalize() + ".hs"
    for root,_,files in os.walk(path):
        for file in files:
            if file == hsName:
                return [os.path.join(root,file)]
    else:
        return []

def ghc_compile_cmd(filename, libname, filedir, optimisation=2):
    RESOURCES = os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir, "res")
    HS_BRACKET_C = os.path.join(RESOURCES,"hswrap.c")

    GHC_CMD = "ghc-" + GHC_VERSION
    GHC_OPT_OPTIMISATION = ["","-O","-O2","-optc-O3"]
    GHC_OPTIONS = ["-dynamic","-shared","-fPIC","-i:"+filedir] # "-fexternal-dynamic-refs"
    GHC_OUT = "-o"
    LIB_HS_RTS = "-lHSrts-ghc" + GHC_VERSION
    return [
        GHC_CMD, GHC_OPT_OPTIMISATION[optimisation], *GHC_OPTIONS,
        GHC_OUT, libname, filename, HS_BRACKET_C, LIB_HS_RTS
        ]

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

def get_exported(hs_file):
    return dict(_get_exported(hs_file))

def _get_exported(hs_file):
    with open(hs_file, 'r') as f:
        for line in f.readlines():
            yield from _exported(line)

def _exported(hs_line):
    if hs_line.startswith('foreign export ccall'):
        func_export,type = hs_line.split('::')
        *_,name = func_export.strip().split(' ')
        yield name, hs_type_to_py(type)