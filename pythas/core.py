from importlib.abc import Loader, MetaPathFinder
from importlib.util import spec_from_file_location
from subprocess import run
from ctypes import cdll
from functools import partial
from sys import meta_path, platform
import os.path

from .haskell.ghc import GHC_VERSION, ghc_compile_cmd
from .haskell import hsparser
from .haskell.parse_file import parse_haskell
from .utils import custom_attr_getter, find_source, DOT

from importlib.abc import MetaPathFinder

class pythasMetaFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target=None):
        if path is None:
            path = [os.getcwd()]

        if DOT in fullname:
            *_,name = fullname.split(DOT)
        else:
            name = fullname

        for p in path:
            # let's assume it's a python module
            subname = os.path.join(p, name)
            if os.path.isdir(subname):
                filename = os.path.join(subname,'__init__.py')
            else:
                filename = subname + '.py'
            # and check if this module exists
            if not os.path.exists(filename):
                # in case it doesn't look for a haskell file of that name
                for haskellfile in find_source(name, p):
                    return spec_from_file_location(fullname, p, loader=pythasLoader(haskellfile),
                        submodule_search_locations=None)

        # Let the other finders handle this
        return None

class pythasLoader(Loader):
    def __init__(self, filename):
        self.filename = filename

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        ffi_filename = hsparser.createFileBindings(self.filename)
        ffi_files = [ffi_filename]
        for f in ffi_files:
            print("Got File: " +f)
        ffi_pinfos = map(parse_haskell,ffi_files)
        libs = [(cdll.LoadLibrary(libname),info)
            for libname,info in create_shared_libs(ffi_files, ffi_pinfos)]
        setattr(module, 'ffi_libs', libs)
        module.__getattr__ = partial(custom_attr_getter, module)

def create_shared_libs(ffi_files, ffi_pinfos):
    yield from (ghc_compile(fn, info) for fn,info in zip(ffi_files, ffi_pinfos))

def ghc_compile(filename, parse_info):
    filedir = parse_info.dir
    name = parse_info.name.lower()
    libname = os.path.join(filedir,'lib'+name)
    if platform.startswith('linux'):
        libname += '.so'
    elif platform.startswith('win32'):
        libname += '.dll'
    cmd = ghc_compile_cmd(filename, libname, filedir, platform)
    run(cmd)
    return libname, parse_info

def install():
    meta_path.insert(0, pythasMetaFinder())
