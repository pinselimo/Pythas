"""Wrapping functionality for GHC and STACK."""

import subprocess
import sys
import re
import os.path
from shutil import which

def has_stack():
    """Looks for stack on the `$PATH`.

    Returns
    -------
    has_stack : bool
        True if stack is in `$PATH`.
    """
    return which('stack') is not None

def get_ghc_version_from_cmdln(stack_ghc):
    """Retrieves the GHC version number from the command line.

    Parameters
    ----------
    stack_ghc : bool
        True if stack's GHC is used.

    Returns
    -------
    version : str
        Version number string.
    """
    REGEX_HS_VERSION = b'(?<=[a-z A-Z])[0-9.]+'

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
    """Retrieves the GHC version number from the `ghcversion.h` header.

    Returns
    -------
    version : str
        Version number string.

    Raises
    ------
    ImportError : Version-Number of GHC could not be found
    """
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
    """Retrieves the GHC version number.
    Defaults to getting it from the command line,
    reverts to the `ghcversion.h` header.

    Parameters
    ----------
    stack_ghc : bool
        True if stack's GHC is used.

    Returns
    -------
    version : str
        Version number string.

    See Also
    --------
    get_ghc_version_from_cmdln
    get_ghc_version_from_header

    Raises
    ------
    ImportError : Version-Number of GHC could not be found
    """
    try:
        return get_ghc_version_from_cmdln(stack_ghc)
    except AttributeError: # Regex didn't match, fallback
        return get_ghc_version_from_header()

def check_ghc_version(stack=has_stack()):
    """Checks if the GHC version required is installed.

    Parameters
    ----------
    stack : bool
        If True check version of stack ghc.

    Raises
    ------
    ImportError : Version-Number of GHC is too low
    """
    ghc_version = get_ghc_version(stack)

    major,minor,micro = ghc_version.split('.')
    if int(major) < 8:
        if stack:
            raise ImportError(
                    'Stack GHC version {} too low.'.format(ghc_version) +
                    'Update it to at least 8.0.2 by changing the stack config file.'
                    )
        else:
            raise ImportError(
                    'GHC version {} too low.'.format(ghc_version) +
                    'Update it to at least 8.0.2 or install stack.'
                    )

class GHC:
    """Pythas interface class for GHC."""
    def compile(self, filepath, libpath, use_stack=has_stack(), more_options=tuple(), _redirect=False):
        """Compile a Haskell source file to a shared library.

        Parameters
        ----------
        filepath : str
            Pathlike object referencing the Haskell source file.
        libpath : str
            Pathlike object referencing the shared library file.
        use_stack : bool
            If True uses stack ghc as compile command if available. Defaults to availability.
        more_options : tuple(str) = ()
            Additional flags handed to GHC.
        _redirect : bool = False
            Internal binaries are redirect into Pythas' bin directory for clean pip uninstall.

        Returns
        -------
        libpath : str
            Pathlike object referencing the shared library path.
        """
        cwd = os.getcwd()
        os.chdir( os.path.dirname(filepath) )
        flags = self.flags(filepath, libpath, use_stack, _redirect)
        flags += more_options
        cmd = self.ghc_compile_cmd(use_stack, flags)

        check_ghc_version()
        print('Compiling with: {}'.format(cmd[0]))
        proc = subprocess.run(cmd, capture_output=True)

        if proc.returncode > 0:
            logfile = os.path.join(cwd, '.pythas.log')
            with open(logfile, 'wb') as f:
                f.write(proc.stdout)
                f.write(proc.stderr)

            raise CompileError(
                        "Stack failed with exit code {} \n"
                        "The log has been written to {}"
                        "".format(proc.returncode, logfile)
                        )
        else:
            os.chdir(cwd)
            return libpath

    def flags(self, filename, libname, use_stack, _redirect=False):
        """Creates the flags needed for successful compilation of Haskell FFI files
        using Pythas.

        Parameters
        ----------
        filepath : str
            Pathlike object referencing the Haskell source file.
        libpath : str
            Pathlike object referencing the shared library file.
        use_stack : bool
            If True uses stack ghc as compile command.
        _redirect : bool = False
            Internal binaries are redirect into Pythas' bin directory for clean pip uninstall.

        Returns
        -------
        flags : tuple(str)
            Flags for compilation of `filepath` to shared library in `libpath`.
        """
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
            ('-hidir', BIN, '-odir', BIN) if _redirect else ())
            # Old options: '-fexternal-dynamic-refs'

        GHC_OUT = '-o'

        if sys.platform.startswith('linux') or sys.platform.startswith('darwin'):
            GHC_OPTIONS = ('-dynamic',) + GHC_OPTIONS
            LIB_HS_RTS = '-lHSrts-ghc' + get_ghc_version(use_stack)
            flags = (
                *GHC_OPTIONS, GHC_OUT, libname, filename,
                *PYTHAS_TYPES, HS_BRACKET_C, LIB_HS_RTS
                )

        elif sys.platform.startswith('win32'):
            flags = (
                *GHC_OPTIONS, GHC_OUT, libname, filename,
                *PYTHAS_TYPES, HS_BRACKET_C
                )

        return flags

    def ghc_compile_cmd(self, use_stack, options):
        """Generates the compile command to GHC.

        Parameters
        ----------
        use_stack : bool
            If True uses stack ghc as compile command.
        options : tuple(str)
            The flags handed to GHC.

        Returns
        -------
        command : tuple(str)
            The entire command to initiate compilation.
        """
        GHC_CMD = 'ghc'
        if use_stack:
            # https://gitlab.haskell.org/ghc/ghc/-/issues/17926
            STACK_OPTIONS = ('--resolver=lts-14.27',) if sys.platform.startswith('win32') else ()

            return ('stack',) + STACK_OPTIONS + (GHC_CMD, '--') + options
        else:
            return (GHC_CMD,) + options

class CompileError(ImportError):
    pass

