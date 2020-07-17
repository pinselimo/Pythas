import os.path
from ctypes import cdll, c_wchar_p, c_voidp
from subprocess import run
from sys import platform

from .ghc import ghc_compile_cmd

dir = os.path.dirname(os.path.realpath(__file__))
dir = os.path.join( dir, "ffi" )
src = os.path.join( dir, "HaskyFFI.hs")
lib = os.path.join( dir, "libhaskyffi")
if not os.path.exists( lib ):
    if platform.startswith('linux'):
        lib += '.so'
    elif platform.startswith('win32'):
        lib += '.dll'
    cmd = ghc_compile_cmd(src,lib,dir,platform,redirect=True)
    run(cmd)

hsparser = cdll.LoadLibrary( lib )
hsparser.createFileBindings.argtypes = [c_wchar_p]
hsparser.createFileBindings.restype = c_wchar_p
hsparser.freeReturnedString.argtypes = [c_wchar_p]
hsparser.freeReturnedString.restype = c_voidp

