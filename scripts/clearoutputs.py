#! /usr/bin/python
"""
Moves recursively through all subdirectories and looks for files which need to be added to the pip install extra-files.
It then clears them of all content and outputs a list of the files.
"""

import os
import os.path

SUFFICES = (".hi", ".o", ".h", ".so")


def clear_files():
    files = []
    for dirpath, dirnames, filenames in os.walk(os.getcwd()):
        for filename in [f for f in filenames if any(f.endswith(s) for s in SUFFICES)]:
            fullname = os.path.join(dirpath, filename)
            open(fullname, "w").close()
            files.append(fullname)

    return files


if __name__ == "__main__":
    files = clear_files()
