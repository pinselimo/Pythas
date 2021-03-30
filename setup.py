from setuptools import setup
import os

DESC = "Import Haskell modules as if they were Python modules"

CLASSIFIERS = [
    "Development Status :: 4 - Beta",
    "License :: OSI Approved :: GNU Library or Lesser General Public License (LGPL)",
    "License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)",
    "License :: OSI Approved :: GNU Lesser General Public License v3 or later (LGPLv3+)",
    "Operating System :: OS Independent",
    "Intended Audience :: Developers",
    "Intended Audience :: Education",
    "Intended Audience :: Science/Research",
    "Programming Language :: Python",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.7",
    "Programming Language :: Python :: 3.8",
    "Programming Language :: Python :: 3.9",
    "Programming Language :: Haskell",
    "Topic :: Scientific/Engineering :: Physics",
    "Topic :: Scientific/Engineering :: Mathematics",
    "Topic :: Scientific/Engineering :: Information Analysis",
    "Topic :: Education",
    "Topic :: Software Development",
    "Typing :: Typed",
]

DIRS = (
    ["res/cstructs-in-haskell/src/Foreign/C/" + s for s in ["", "Structs/"]]
    + ["res/Pythas-Types/src/Foreign/Pythas/", "res/", "ffi/src/", "bin/"]
    + ["ffi/src/Foreign/" + s for s in ["", "Pythas/"]]
    + ["bin/Foreign/" + s for s in ["", "C/", "Pythas/", "C/Structs/"]]
)

SUFFICES = [".hs", ".hi", ".o", ".so", ".c", ".h"]

with open("README.md", "r") as f:
    LONG_DESCRIPTION = f.read()

if os.getenv("READTHEDOCS") == "True":
    INSTALL_REQUIRES = []
else:
    with open("requirements.txt") as f:
        INSTALL_REQUIRES = [r.strip() for r in f.readlines()]

setup(
    name="pythas",
    version="0.1b1",
    description=DESC,
    long_description=LONG_DESCRIPTION,
    long_description_content_type="text/markdown",
    author="Simon Plakolb",
    author_email="s.plakolb@gmail.com",
    license="LGPLv3",
    platforms="any",
    url="https://github.com/pinselimo/Pythas",
    packages=["pythas", "pythas.parser", "pythas.haskell"],
    package_dir={
        "pythas": "pythas",
        "pythas.parser": "pythas/parser",
        "pythas.haskell": "pythas/haskell",
    },
    package_data={"pythas.haskell": [d + "*" + s for d in DIRS for s in SUFFICES]},
    install_requires=INSTALL_REQUIRES,
    python_requires=">=3.7",
    classifiers=CLASSIFIERS,
)
