"""Parse Haskell modules/files."""

import os.path
from logging import getLogger

from .data import ParseInfo, FuncInfo
from .parse_type import parse_type

logger = getLogger(__name__)


def parse_haskell(hs_file):
    """Parses a Haskell file for exported functions and ffi exports.

    Parameters
    ----------
    hs_file : str
        Pathlike object referring to the Haskell source file.

    Reuturns
    --------
    parse_info : ParseInfo
        Informations parsed from `hs_file`.
    """
    # preprocessing of file
    *path, name = os.path.split(hs_file)
    name = name[: name.find(".hs")]
    filedir = os.path.join(*path)
    parse_info = ParseInfo(name, filedir, set(), set(), dict())

    with open(hs_file, "r") as f:
        contents = f.readlines()
    parse_info = _parse_haskell(contents, parse_info)

    exported_mod = parse_head(contents, name)
    if exported_mod:
        parse_info.exported_mod.update(exported_mod)
    else:
        parse_info.exported_mod.update(
            set(parse_info.func_infos.keys()) - parse_info.exported_ffi
        )

    return parse_info


def _parse_haskell(hs_lines, parse_info):
    """Parses lines of a Haskell source file.

    Parameters
    ----------
    hs_lines : list(str)
        Lines of a Haskell source file.
    parse_info : ParseInfo
        Container into which informations are to be stored.

    Returns
    -------
    parse_info : ParseInfo
        Informations parsed from `hs_lines`.
    """
    in_comment = False
    for line_nr, hs_line in enumerate(hs_lines):
        for hs_line in hs_line.split(";"):
            # Pre-processing of hs_line
            hs_line = hs_line.strip()
            in_comment = "{-" in hs_line
            if not (in_comment or hs_line.startswith("\n") or hs_line.startswith("--")):
                parse_line(line_nr + 1, hs_line, parse_info)

            elif in_comment:
                in_comment = not "-}" in hs_line

    return parse_info


def find_module_statement(hs_cont, name):
    """Locates the `module` statement in a Haskell source file.

    Parameters
    ----------
    hs_cont : str
        Content of a Haskell source file.
    name : str
        Name of the Haskell module as given by file name.

    Returns
    -------
    posiiton : int
        Index of the `module` statement in the `hs_cont`.

    Raises
    ------
    SyntaxError : Haskell file module statement malformed
    """
    module_name = "module {}".format(name)
    module_decl = hs_cont.find(module_name)

    if module_decl > -1:
        return module_decl + len(module_name)
    else:
        raise SyntaxError("Haskell file module statement malformed (Case sensitive!)")


def parse_head(hs_lines, name):
    """Finds all the names that are exported according to the module statement.

    Parameters
    ----------
    hs_lines : list(str)
        Lines of a Haskell source file
    name : str
        Name of the Haskell module as given by file name.

    Returns
    -------
    exports : list(str)
        List of exported names or `None` if no exports statement is given.
    """
    hs_cont = " ".join(hs_lines)
    name, *_ = name.split(".")
    module_decl_end = find_module_statement(hs_cont, name)
    where = hs_cont.find("where")

    head = hs_cont[module_decl_end:where].strip()
    if len(head) == 0:
        logger.info("No export restrictions found")
        return None
    else:
        return {n.strip() for n in head.strip("() \n").split(",") if len(n) > 0}


def parse_line(line_nr, hs_line, parse_info):
    """Parses a single line of Haskell source code.

    Parameters
    ----------
    linr_nr : int
        Line number of the current line.
    hs_line : str
        Line of Haskell code.
    parse_info : ParseInfo
        Container into which informations are to be stored.
    """
    hs_line = hs_line.strip(" ;")
    if hs_line.startswith("foreign export ccall"):
        func_export, type_def = hs_line.split("::")
        *_, name = func_export.strip().split(" ")
        name = name.strip()

        parse_info.exported_ffi.add(name)
        parse_info.func_infos[name] = parse_type(line_nr, name, type_def)
