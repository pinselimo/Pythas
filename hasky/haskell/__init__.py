import os.path
from ctypes import cdll, c_wchar_p, c_voidp
from subprocess import run
from sys import platform

from .ghc import ghc_compile_cmd

dir_path = os.path.dirname(os.path.realpath(__file__))
libparsehs = os.path.join( dir_path, "libhaskyffi.so")

dir = os.path.dirname(os.path.realpath(__file__))
src = os.path.join( dir, "Exports.hs")
lib = os.path.join( dir, "libhaskyffi")
if not os.path.exists( lib ):
    if platform.startswith('linux'):
        lib += '.so'
    elif platform.startswith('win32'):
        lib += '.dll'
    cmd = ghc_compile_cmd(src,lib,dir,platform)
    run(cmd)

hsparser = cdll.LoadLibrary( libparsehs )
hsparser.createFileBindings.argtypes = [c_wchar_p]
hsparser.createFileBindings.restype = c_wchar_p
hsparser.freeReturnedString.argtypes = [c_wchar_p]
hsparser.freeReturnedString.restype = c_voidp
