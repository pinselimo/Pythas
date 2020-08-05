from importlib.abc import Loader, MetaPathFinder
from importlib.util import spec_from_file_location
from subprocess import run
from functools import partial, reduce
from sys import meta_path, platform
import os.path

from .utils import custom_attr_getter, find_source

from importlib.abc import MetaPathFinder

class PythasMetaFinder(MetaPathFinder):
    def __init__(self, compiler):
        self.compiler = compiler

    def find_spec(self, fullname, path, target=None):
        if path is None:
            path = [os.getcwd()]

        if '.' in fullname:
            *_,name = fullname.split('.')
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
                    return spec_from_file_location(
                            fullname,
                            p,
                            loader=PythasLoader(self.compiler, haskellfile),
                            submodule_search_locations=None
                            )

        # Let the other finders handle this
        return None

class PythasLoader(Loader):
    def __init__(self, compiler, filename):
        self.compiler = compiler
        self.filename = filename

    def exec_module(self, module):
        lib, ffi_pinfos = self.compiler.compile(self.filename)

        # Duck typing for custom_attr_getter
        module._ffi_libs = [(lib, ffi_pinfos)]

        module.__getattr__ = partial(custom_attr_getter, module)
        module.__dir__ = lambda: list(module.__dict__) + list(ffi_pinfos.exported_ffi)

def install(compiler):
    meta_path.insert(0, PythasMetaFinder(compiler))

