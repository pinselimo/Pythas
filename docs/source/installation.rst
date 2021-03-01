.. _pythas_installation:

Installation
============

While *Pythas* itself is a Python library, it contains a considerable amount of Haskell source code. This part of the library is compiled once at the first import. Therefore, it is paramount to provide a *GHC* installation. Note that in subsequent usages this compilation is not necessary, reducing the import time significantly.
If you already have either *GHC* or *Stack* (or both) installed, skip ahead to :ref:`pythas_package_installation`.

Install Haskell
---------------

For compilation of Haskell source *Pythas* makes use of the *Glasgow Haskell Compiler (GHC)*. This is the most commonly used compiler for the Haskell programming language. It comprises a multitude of extensions to the language and most existing Haskell code should therefore be supported by *Pythas*. However, thorough support of all GHC language extensions cannot be guaranteed and is to be considered experimental at this stage (|version|).

Cabal/GHC vs Stack
^^^^^^^^^^^^^^^^^^

The GHC compiler can be installed in various ways (without claim to completeness):

  + As a bare bones `compiler executable <https://www.haskell.org/ghc/download.html>`_.
  + As a `minimal package <https://www.haskell.org/downloads/#minimal>`_ also including the build tools *Cabal* and *Stack*.
  + On top of the `Stack <https://docs.haskellstack.org/en/stable/README/>`_ build tool providing management of different GHC installs.
  + With the full blown `Haskell Platform <https://www.haskell.org/platform/>`_ installation providing the most common libraries and build tools altogether.

If a *Stack* install is available, *Pythas* will default to utilizing it. This means that available ``stack.yaml`` files are taken into consideration providing stable and consistent compilation results.
This choice implies that any other *GHC* installations are ignored if the ``stack`` command is found to be available. If you prefer to use ``ghc`` instead you can tell *Pythas* to disable ``stack``:

.. code-block:: python

    from pythas import compiler
    compiler.stack_usage = False
    print("Stack is used: {}".format(compiler.stack_usage))

This will use whichever ``ghc`` is in the path of the directory the source file is located in. Using *Cabal* you can thus add the ``--write-ghc-environment-files=always`` option when using ``cabal build``. This will create a local ``.ghc.environment`` directory with its own *GHC* installation and link it to the local ``ghc`` command. Then this local instance of *GHC* and its libraries will also be utilized by *Pythas*.

.. _pythas_package_installation:

Install Pythas
--------------

Once either ``stack`` and/or *GHC* are installed, you can install *Pythas*.

From pip
^^^^^^^^

Pythas is available for download from the Python Package Index ``pypi`` via ``pip``::

    $ pip install pythas
    $ python -c "import pythas" # Will compile the Haskell source

From source
^^^^^^^^^^^

If you want to be at the newest stage of development, you can install this package from source using the following commands::

    $ git clone --recurse-submodules -j8 https://github.com/pinselimo/Pythas.git
    $ cd Pythas && pip install .
    $ cd ..
    $ python -c "import pythas" # Will compile the Haskell source

