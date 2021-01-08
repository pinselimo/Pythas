Contributing
============

The source code of *Pythas* is split among multiple repositories:

* The `main repository <https://github.com/pinselimo/Pythas>`_ contains the Python source handling all the interaction with the Python runtime system.
* `Pythas-FFI <https://github.com/pinselimo/Pythas-FFI>`_ contains the backend responsible for parsing Haskell modules and transpiling them into FFI exports.
* `Pythas-Types <https://github.com/pinselimo/Pythas-Types>`_ defines the custom Haskell types required to exchange nested data types in between the two languages. Their Python equivalents are defined in ``pythas.types``.
* `C-structs <https://github.com/pinselmo/cstructs-in-haskell>`_ is a Haskell package for variably typed, correctly aligned C structs.

Contributions are welcome in all of these repositories. Please be advised to checkout the respective *CONTRIBUTING.md* file first. The preferred contribution work flow is to first raise an issue on github, then issue a pull request only after the issue's discussion was successful.

License
=======

Most code of the *Pythas* package is licensed under the LGPLv3 license. This license is valid for all source code if not explicitly stated otherwise. Some parts are licensed under the MIT license, notably the *C-structs* Haskell package. Please refer to the respective *COPYING* and *COPYING.LESSER* or *LICENSE* files for details.

