import subprocess
import sys
import re
import os.path
from shutil import which

def get_ghc_version_from_cmdln(stack_ghc):
    REGEX_HS_VERSION = b'(?<=[a-z A-Z])[0-9.]{5}'

    if stack_ghc:
        cmd = ('stack','ghc','--','--version')
    else:
        cmd = ('ghc','--version')

    py_vn = sys.version_info
    if py_vn == 3 and py_vn.minor > 6:
        stdout = subprocess.run(cmd,capture_output=True).stdout
    else:
        stdout = subprocess.run(cmd,stdout=subprocess.PIPE).stdout

    version = re.search(REGEX_HS_VERSION, stdout )
    return version.group(0).decode('utf-8')

def get_ghc_version_from_header():
    GHC_VERSION_H = '/usr/lib/ghc/include/ghcversion.h'
    REGEX_C_CONSTANTS = '#define[ \t\n\r\f\v]+([a-zA-Z0-9_]+)[ \t\n\r\f\v]+([0-9]+)'

    consts = {}
    with open(GHC_VERSION_H,'r') as header:
        for name, value in re.findall(REGEX_C_CONSTANTS, header.read()):
            consts[name] = value

            try:
                pl = consts['__GLASGOW_HASKELL_PATCHLEVEL1__']
            except KeyError:
                pl = '0'

            try:
                vn = consts['__GLASGOW_HASKELL__']
                v = str(int(vn)//100)
                n = str(int(vn)%100)
                return v+'.'+n+'.'+pl

            except KeyError:
                raise ImportError('Version-Number of GHC could not be found')

def get_ghc_version(stack_ghc):
    try:
        return get_ghc_version_from_cmdln(stack_ghc)
    except AttributeError: # Regex didn't match, fallback
        return get_ghc_version_from_header()

def has_stack():
    return which('stack') is not None

class GHC:
    def __init__(self):
        self._stack = has_stack()
        self._optimisation = 2

    @property
    def VERSION(self):
        return get_ghc_version(self._stack)

    @property
    def optimisation(self):
        return self._optimisation

    @optimisation.setter
    def optimisation(self, level):
        self._optimisation = min(2, max(0, level))

    def compile(self, filepath, libpath, more_options=tuple(), redirect=False):
        cwd = os.getcwd()
        os.chdir( os.path.dirname(filepath) )
        flags = self.flags(filepath, libpath, redirect)
        flags += more_options
        cmd = self.ghc_compile_cmd(flags)

        print('Compiling with: {}'.format(cmd[0]))
        subprocess.run(cmd)

        os.chdir(cwd)
        return libpath

    def flags(self, filename, libname, redirect=False):
        fdir = os.path.dirname(os.path.abspath(__file__))

        RESOURCES = os.path.join(fdir, 'res')
        BIN = os.path.join(fdir, 'bin')

        HS_BRACKET_C = os.path.join(RESOURCES, 'hswrap.c')
        PATH_CSTRUCTS = (RESOURCES, 'cstructs-in-haskell', 'src', 'Foreign', 'C')
        PATH_PYTHASTYPES = (RESOURCES, 'Pythas-Types', 'src', 'Foreign', 'Pythas')
        PYTHAS_TYPES = [
                os.path.join(*PATH_PYTHASTYPES,t)
                for t in ('Array.hs', 'List.hs', 'String.hs', 'Tuples.hs', 'Templates.hs')
                ] \
                + [
                os.path.join(*PATH_CSTRUCTS, 'Structs', t)
                for t in ('Types.hs', 'Templates.hs', 'Utils.hs')
                ] \
                + [os.path.join(*PATH_CSTRUCTS, 'Structs.hs')]

        GHC_OPTIONS = ('-shared', '-fPIC', '-i:'+os.path.dirname(filename)) + (
            # We redirect our own binaries into the 'bin' dir to not pollute everything
            ('-hidir', BIN, '-odir', BIN) if redirect else ())
            # Old options: '-fexternal-dynamic-refs'

        GHC_OPT_OPTIMISATION = ['', '-O', '-O2', '-optc-O3']
        GHC_OUT = '-o'

        if sys.platform.startswith('linux') or sys.platform.startswith('darwin'):
            GHC_OPTIONS = ('-dynamic',) + GHC_OPTIONS
            LIB_HS_RTS = '-lHSrts-ghc' + self.VERSION
            flags = (
                GHC_OPT_OPTIMISATION[self.optimisation], *GHC_OPTIONS,
                GHC_OUT, libname, filename, *PYTHAS_TYPES, HS_BRACKET_C, LIB_HS_RTS
                )

        elif sys.platform.startswith('win32'):
            flags = (
                GHC_OPT_OPTIMISATION[self.optimisation], *GHC_OPTIONS,
                GHC_OUT, libname, filename, *PYTHAS_TYPES, HS_BRACKET_C
                )

        return flags

    def ghc_compile_cmd(self, options):
        GHC_CMD = 'ghc'
        if self._stack:
            # https://gitlab.haskell.org/ghc/ghc/-/issues/17926
            STACK_OPTIONS = ('--resolver=lts-14.27',) if sys.platform.startswith('win32') else ()

            return ('stack',) + STACK_OPTIONS + (GHC_CMD, '--') + options
        else:
            return (GHC_CMD,) + options
