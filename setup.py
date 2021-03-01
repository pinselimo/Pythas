from setuptools import setup

dirs = (
    ["res/cstructs-in-haskell/src/Foreign/C/" + s for s in ["", "Structs/"]]
    + ["res/Pythas-Types/src/Foreign/Pythas/", "res/", "ffi/src/", "bin/"]
    + ["ffi/src/Foreign/" + s for s in ["", "Pythas/"]]
    + ["bin/Foreign/" + s for s in ["", "C/", "Pythas/", "C/Structs/"]]
)

suffices = [".hs", ".hi", ".o", ".so", ".c", ".h"]

with open("README.md", "r") as f:
    long_description = f.read()

with open("requirements.txt") as f:
    requirements_lines = f.readlines()
install_requires = [r.strip() for r in requirements_lines]

import pythas

setup(
    name="Pythas",
    version="0.01",
    description="Import Haskell modules as if they were Python modules",
    license="LGPLv3",
    long_description=long_description,
    long_description_content_type="text/markdown",
    author="Simon Plakolb",
    author_email="s.plakolb@gmail.com",
    url="https://www.behaviour.space/",
    packages=["pythas", "pythas.parser", "pythas.haskell"],
    package_dir={
        "pythas": "pythas",
        "pythas.parser": "pythas/parser",
        "pythas.haskell": "pythas/haskell",
    },
    package_data={"pythas.haskell": [d + "*" + s for d in dirs for s in suffices]},
    install_requires=install_requires,
)
