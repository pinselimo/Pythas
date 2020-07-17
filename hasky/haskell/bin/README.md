# Bin directory for GHC created binaries

This directory contains dummy files for the ```.o``` and ```.hi``` files created on compilation of ```../ffi/CreateFFI.hs```. We want those to be there for ```pip``` but not to clutter our code directories.

### Explanation:

When Hasky is imported for the first time, or updated the underlying library for parsing Haskell and creating the FFI file needs to be (re-)compiled. During compilation GHC creates some binary interface and object files. On uninstall, pip wouldn't be able to delete these files due to their inexistance on install.
If it sees the dummy files and it installs them too and can later remove the actual binaries.

