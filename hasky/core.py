from importlib.abc import Loader, MetaPathFinder
from importlib.util import spec_from_file_location
from subprocess import run
from ctypes import cdll
from functools import partial
from sys import meta_path, platform
import os.path

from .haskell.ghc import GHC_VERSION, ghc_compile_cmd
from .haskell.createffi import create_ffi
from .haskell.utils import get_exported
from .utils import custom_attr_getter, findSource, DOT

from importlib.abc import MetaPathFinder

class HaskyMetaFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target=None):
        path = os.getcwd()

        if DOT in fullname:
            *pack,name = fullname.split(DOT)
        else:
            name = fullname
            pack = []
        path = os.path.join(path,*pack)
        # let's assume it's a python module
        subname = os.path.join(path, name)
        if os.path.isdir(subname):
            filename = os.path.join(subname,'__init__.py')
        else:
            filename = subname + '.py'
        # and check if this module exists
        if not os.path.exists(filename):
            # in case it doesn't look for a haskell file of that name
            for haskellfile in findSource(name, path):
                return spec_from_file_location(name, haskellfile, loader=HaskyLoader(haskellfile),
                    submodule_search_locations=None)

        # Let the other finders handle this
        return None 

class HaskyLoader(Loader):
    def __init__(self, filename):
        self.filename = filename

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        libs = [(cdll.LoadLibrary(libname),funcs)
            for libname,funcs in create_shared_libs(self.filename)]
        setattr(module, 'ffi_libs', libs)
        module.__getattr__ = partial(custom_attr_getter,module)

def create_shared_libs(filename):
    yield from (ghc_compile(fn) for fn in create_ffi(filename))

def ghc_compile(filename):
    filedir = os.path.dirname(filename)
    name = os.path.basename(filename).split('.')[0].lower()
    libname = os.path.join(filedir,'lib'+name)
    if platform.startswith('linux'):
        libname += '.so'
    elif platform.startswith('win32'):
        libname += '.dll'
    cmd = ghc_compile_cmd(filename, libname, filedir, platform)
    run(cmd)
    return libname, get_exported(filename)

def install():
    meta_path.insert(0, HaskyMetaFinder())
