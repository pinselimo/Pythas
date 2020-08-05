from .utils import check_has_ghc
check_has_ghc()

from .core import install
from .compiler import compiler, SourceModule

install(compiler)

