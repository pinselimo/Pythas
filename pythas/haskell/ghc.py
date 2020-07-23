import subprocess
import re
import os.path
from shutil import which

REGEX_HS_VERSION = b'(?<=[a-z A-Z])[0-9.]{5}'
REGEX_C_CONSTANTS = '#define[ \t\n\r\f\v]+([a-zA-Z0-9_]+)[ \t\n\r\f\v]+([0-9]+)'

GHC_VERSION_H = '/usr/lib/ghc/include/ghcversion.h'

__GLASGOW_HASKELL__ = '__GLASGOW_HASKELL__'
__GLASGOW_HASKELL_PATCHLEVEL1__ = '__GLASGOW_HASKELL_PATCHLEVEL1__'

class GHC_Exception(Exception):
    pass

def get_ghc_version_from_cmdln():
    stdout = subprocess.run(['ghc','--version'],capture_output=True).stdout
    version = re.search(REGEX_HS_VERSION, stdout )
    return version.group(0).decode('utf-8')

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
                raise GHC_Exception('Version-Number of GHC could not be found')

def get_ghc_version():
    try:
        return get_ghc_version_from_cmdln()
    except AttributeError: # Regex didn't match, fallback
        return get_ghc_version_from_header()

def has_stack():
    return which('stack') is not None

def ghc_compile_cmd(filename, libname, filedir, platform, optimisation=2, redirect=False):
    fdir = os.path.dirname(os.path.abspath(__file__))
    RESOURCES = os.path.join(fdir, 'res')
    BIN = os.path.join(fdir, 'bin')
    HS_BRACKET_C = os.path.join(RESOURCES,'hswrap.c')
    GHC_OPT_OPTIMISATION = ['','-O','-O2','-optc-O3']
    GHC_OUT = '-o'
    PATH_CSTRUCTS = ('cstructs-in-haskell','src','Foreign','C')
    PATH_PYTHASTYPES = (RESOURCES,'Pythas-Types','src','Foreign','Pythas')
    PYTHAS_TYPES = [os.path.join(*PATH_PYTHASTYPES,t)
            for t in ['Array.hs','List.hs','String.hs','Tuples.hs',]] \
            + [os.path.join(RESOURCES,*PATH_CSTRUCTS,'Structs.hs')] \
            + [os.path.join(RESOURCES,*PATH_CSTRUCTS,'Structs','Types.hs')] \
            + [os.path.join(RESOURCES,*PATH_CSTRUCTS,'Structs','Utils.hs')]

    # We redirect our own binaries into the 'bin' dir to not pollute everything
    if redirect:
        OUTP = ('-hidir',BIN,'-odir',BIN)
    else:
        OUTP = ()
    GHC_OPTIONS = ('-shared','-fPIC','-i:'+filedir) + OUTP # '-fexternal-dynamic-refs'

    STACK_CMD = 'stack'
    WITH = '--'
    GHC_CMD = 'ghc'

    if platform.startswith('linux'):
        GHC_OPTIONS = ('-dynamic') + GHC_OPTIONS
        LIB_HS_RTS = '-lHSrts-ghc' + GHC_VERSION
        flags = (
            GHC_OPT_OPTIMISATION[optimisation], *GHC_OPTIONS,
            GHC_OUT, libname, filename, *PYTHAS_TYPES, HS_BRACKET_C, LIB_HS_RTS
            )
    elif platform.startswith('win32'):
        # https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/win32-dlls.html
        flags = (
            GHC_OPT_OPTIMISATION[optimisation], *GHC_OPTIONS,
            GHC_OUT, libname, filename, *PYTHAS_TYPES, HS_BRACKET_C
            )

    if has_stack():
        return (STACK_CMD, GHC_CMD, WITH) + flags
    else:
        return (GHC_CMD,) + flags

GHC_VERSION = get_ghc_version()

