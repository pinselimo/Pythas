Installation
------------

While ``Pythas`` itself is a Python library, it contains a considerable amount of Haskell source code. This part of the library is compiled once at the first import. Therefore, it is paramount to provide a ``GHC`` installation. Note that in subsequent usages this compilation is not necessary, reducing the import time significantly.

Haskell compilation
-------------------

For compilation of Haskell source ``Pythas`` makes use of the Glasgow Haskell Compiler (GHC). This is the most commonly used compiler for the Haskell programming language. It comprises a multitude of extensions to the language which should therefore mostly be supported by ``Pythas``. However, thorough support of all GHC language extensions cannot be guaranteed and is to be considered experimental at this stage (v0.01dev).

Cabal/GHC vs Stack
------------------

The GHC compiler can be installed in various ways.

  + As a bare bones compiler executable (`Link https://www.haskell.org/ghc/download.html`_)
  + As a minimal package also including build tools ``Cabal`` and ``Stack`` (`Link https://www.haskell.org/downloads/#minimal`_)
  + On top of the ``Stack`` build tool providing management of different GHC installs (`Link https://docs.haskellstack.org/en/stable/README/`_)
  + With the full blown ``Haskell Platform`` installation providing the most common libraries and build tools altogether (`Link https://www.haskell.org/platform/`_)

If a ``Stack`` install is available, ``Pythas`` will default to utilizing it. This means that available ``stack.yaml`` files are taken into consideration providing stable and consistent compilation results.

This choice implies that any other ``GHC`` installs are ignored if the ``stack`` command is found to be available. If you prefer to use ``ghc`` instead you can tell ``Pythas`` to disable ``stack``:

.. code-block:: python
    from pythas import compiler
    uses_stack = compiler.ghc.stack_usage(False)
    print("Stack is used: {}".format(uses_stack))

This will use whichever ``ghc`` is in the path of the directory the source file is located in. Using ``Cabal`` you can thus add the ``--write-ghc-environment-files=always`` option when using ``cabal build``. This will create a local ``.ghc.environment`` directory with its own ``GHC`` installation and link it to the local ``ghc`` command.

Pythas from source
------------------

Once either ``stack`` and/or ``GHC`` are installed, you can install ``Pythas`` from source::

    $ git clone --recurse-submodules -j8 https://github.com/pinselimo/Pythas.git
    $ cd Pythas && pip install .
    $ cd ~
    $ python -c "import pythas" # Will compile the Haskell source

Pythas from pip
---------------

Pythas is also available for download from the Python Package Index ``pypi`` via ``pip``::

    $ pip install pythas
    $ python -c "import pythas" # Will compile the Haskell source
