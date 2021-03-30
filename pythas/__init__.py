"""
**Pythonically import Haskell modules into your Python context.**

Pythas will automatically find a Haskell module and parse it. It will then create a module containing calls to the Haskell FFI wrapping everything compatible in the imported module. These calls are subsequently compiled into a shared library and parsed by the Python part of Pythas. Ultimately, Pythas imports this shared library as a module-like object into Python and wraps the function calls accordingly. In effect, for the user the calls to the Haskell runtime are almost indistinguishable to common Python calls.

Submodules
^^^^^^^^^^

haskell : Containing all source written in Haskell and a wrapper around the GHC instance.

parser : A basic parser for exported Haskell type declarations.

"""

__version__ = "0.1b1"
__author__ = "Simon Plakolb"
__email__ = "s.plakolb@gmail.com"
__copyright__ = "Copyright 2020, Simon Plakolb"
__license__ = "LGPLv3"
__status__ = "Beta"

from .utils import check_has_ghc, building_docs

if not building_docs():
    check_has_ghc()

from .core import install
from .compiler import compiler, SourceModule
from .parser.utils import TypeWarning

if not building_docs():
    install(compiler)
