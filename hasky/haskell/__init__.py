import os.path
from ctypes import cdll
from subprocess import run
from sys import platform

from .ghc import ghc_compile_cmd

dir_path = os.path.dirname(os.path.realpath(__file__))
libparsehs = os.path.join( dir_path, "libparsehs.so")
if not os.path.exists( libparsehs ):
    dir = os.path.dirname(os.path.realpath(__file__))
    src = os.path.join( dir, "Exports.hs")
    lib = os.path.join( dir, "libparsehs")
    if not os.path.exists( lib ):
        if platform.startswith('linux'):
            lib += '.so'
        elif platform.startswith('win32'):
            lib += '.dll'
        cmd = ghc_compile_cmd(src,lib,dir,platform)
        run(cmd)
hsparser = cdll.LoadLibrary( libparsehs )
