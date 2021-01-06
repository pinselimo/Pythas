"""Pythas init file"""

__version__    = '0.01dev'
__author__     = 'Simon Plakolb'
__email__      = 's.plakolb@gmail.com'
__copyright__  = 'Copyright 2020, Simon Plakolb'
__license__    = 'LGPLv3'
__status__     = 'Development'

from .utils import check_has_ghc

check_has_ghc()

from .core import install
from .compiler import compiler, SourceModule
from .parser.utils import TypeWarning

install(compiler)

