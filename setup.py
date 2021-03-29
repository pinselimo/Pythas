from setuptools import setup

DESC = "Import Haskell modules as if they were Python modules"

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
    version="0.1.0",
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
)
