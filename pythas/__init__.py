from .utils import check_has_ghc
check_has_ghc()

from .core import install
from .context import Context
from .haskell import GHC, ffi_creator

ghc = GHC()
context = Context(ffi_creator, ghc)

install(context)

