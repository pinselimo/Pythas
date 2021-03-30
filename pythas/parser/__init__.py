"""
The parser module provides the minimal parsing capability necessary
to derive the types of the imported functions and constants. Note that
the parser responsible for parsing the original Haskell source is
implemented in Haskell. It can be found in the haskell module's ffi directory.

The parser within this module is strictly limited to read "foreign export ccall"
type annotations.
"""

from .parse_file import parse_haskell
