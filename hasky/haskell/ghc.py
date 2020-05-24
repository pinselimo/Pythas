import subprocess
import re
import os.path

REGEX_HS_VERSION = b'(?<=[a-z A-Z])[0-9.]{5}'
REGEX_C_CONSTANTS = '#define[ \t\n\r\f\v]+([a-zA-Z0-9_]+)[ \t\n\r\f\v]+([0-9+])' # This would cause warning: '#define\s+(\w+)\s+([0-9]+)'

GHC_VERSION_H = '/usr/lib/ghc/include/ghcversion.h'

__GLASGOW_HASKELL__ = "__GLASGOW_HASKELL__"
__GLASGOW_HASKELL_PATCHLEVEL1__ = "__GLASGOW_HASKELL_PATCHLEVEL1__"

class GHC_Exception(Exception):
    pass

def get_ghc_version_from_cmdln():
    stdout = subprocess.run(["ghc","--version"],capture_output=True).stdout
    version = re.search(REGEX_HS_VERSION, stdout )
    return version.group(0).decode("utf-8")
        
def get_ghc_version_from_header():
    consts = {}
    with open(GHC_VERSION_H,'r') as header:
        for name, value in re.findall(REGEX_C_CONSTANTS, header.read()):
            consts[name] = value
            try:
                pl = consts[__GLASGOW_HASKELL_PATCHLEVEL1__]
            except KeyError:
                pl = '0'
            try:
                vn = consts[__GLASGOW_HASKELL__]
                v = str(int(vn)//100)
                n = str(int(vn)%100)
                return v+'.'+n+'.'+pl
            except KeyError:
                raise GHC_Exception("Version-Number of GHC could not be found")

def get_ghc_version():
    try:
        return get_ghc_version_from_cmdln()
    except AttributeError: # Regex didn't match, fallback
        return get_ghc_version_from_header()

def ghc_compile_cmd(filename, libname, filedir, optimisation=2):
    RESOURCES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "res")
    HS_BRACKET_C = os.path.join(RESOURCES,"hswrap.c")

    GHC_CMD = "ghc-" + GHC_VERSION
    GHC_OPT_OPTIMISATION = ["","-O","-O2","-optc-O3"]
    GHC_OPTIONS = ["-dynamic","-shared","-fPIC","-i:"+filedir] # "-fexternal-dynamic-refs"
    GHC_OUT = "-o"
    LIB_HS_RTS = "-lHSrts-ghc" + GHC_VERSION
    return [
        GHC_CMD, GHC_OPT_OPTIMISATION[optimisation], *GHC_OPTIONS,
        GHC_OUT, libname, filename, HS_BRACKET_C, LIB_HS_RTS
        ]

GHC_VERSION = get_ghc_version()