.. Pythas documentation master file, created by
   sphinx-quickstart on Mon Aug 17 19:40:45 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Pythas' documentation!
==================================

With ``Pythas`` it is possible to import Haskell modules into Python just as if they were native modules. It automatically creates the FFI exports including all relevant type conversions, compiles and binds your Haskell code to the Python at runtime.

Dependencies
------------

One development goal for ``Pythas`` was to keep users out of cabal hell. Thus, the pacakage includes all Haskell and C source files needed. To compile its own source and other Haskell modules ``Pythas`` needs either **Stack** or a plain **GHC** installation.
If either of those is installed they can be detected automatically. If **Stack** is installed, it will always be used as default.

Basic Usage
-----------

With an ``example/Example.hs`` file containing a Hello World function ``hello :: IO ()``::
    >>> import pythas
    >>> import example.example as e
    >>> e.hello()
    Hello from Haskell!

For further usage scenarios refer to pythas_usage_.

Contributing
------------

The source code of ``Pythas`` is split among multiple repositories.
* The `main repository <https://github.com/pinselimo/Pythas>` contains the Python source handling all the interaction with the Python runtime system.
* `Pythas-FFI <https://github.com/pinselimo/Pythas-FFI>` contains the backend responsible for parsing Haskell modules and transpiling them into FFI exports. 
* `Pythas-Types <https://github.com/pinselimo/Pythas-Types>` defines the custom Haskell types required to exchange nested data types in between the two languages. Their Python equivalents are defined in ``pythas.types``.
* `C-structs <https://github.com/pinselmo/cstructs-in-haskell>` is a Haskell package for variably typed, correctly aligned C structs.

Contributions are welcome in all of these repositories. Please be advised to checkout the respective ``CONTRIBUTING.md`` file first. The preferred contribution work flow is to first raise an issue on github, then issue a pull request only after the issue's discussion was successful.

License
-------

Most code of the ``Pythas`` package is licensed under the LGPLv3 license. This license is valid for all source code if not explicitly stated otherwise. Some parts are licensed under the MIT license, notably the ``C-structs`` Haskell package. Please refer to the respective ``COPYING`` and ``COPYING.LESSER`` or ``LICENSE`` files for details.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   reference

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
