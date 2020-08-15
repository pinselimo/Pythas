from importlib.abc  import Loader, MetaPathFinder
from importlib.util import spec_from_file_location
from functools  import partial
import os.path
import sys

from .utils import custom_attr_getter, find_source, ffi_libs_exports

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
            subname = os.path.join(p, name)
            if os.path.isdir(subname):
                filename = os.path.join(subname, '__init__.py')
            else:
                filename = subname + '.py'

            if not os.path.exists(filename):
                for haskellfile in find_source(name, p):
                    # Catch and handle Haskell modules
                    return spec_from_file_location(
                              fullname
                            , p
                            , loader=PythasLoader(self.compiler, haskellfile)
                            , submodule_search_locations=None
                            )

        # Let other finders handle the request
        return None

class PythasLoader(Loader):
    def __init__(self, compiler, filename):
        self.compiler = compiler
        self.filename = filename

    def exec_module(self, module):
        ffi_libs = self.compiler.compile(self.filename)
        module._ffi_libs = ffi_libs

        module.__getattr__ = partial(custom_attr_getter, module)
        module.__dir__ = lambda: list(module.__dict__) + list(ffi_libs_exports(ffi_libs))

def install(compiler):
    sys.meta_path.insert(0, PythasMetaFinder(compiler))

