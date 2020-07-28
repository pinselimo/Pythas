from .utils import check_has_ghc
check_has_ghc()

from .core import install
from .context import Context

context = Context()

install(context)

