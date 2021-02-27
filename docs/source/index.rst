.. Pythas documentation master file, created by
   sphinx-quickstart on Mon Aug 17 19:40:45 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Pythas' documentation!
==================================

With *Pythas* it is possible to import Haskell modules into Python just as if they were native modules. It automatically creates the FFI exports including all relevant type conversions, compiles and binds your Haskell code to Python at runtime. It was designed with ease of use in mind and sports a pythonic, intuitive interface.

Dependencies
------------

One development goal for *Pythas* was to keep users out of Haskell's package managing details ("cabal hell"). Thus, the package ships with all Haskell and C source files it needs. To compile its own source and other Haskell modules *Pythas* needs either *Stack* or a plain *GHC* installation. For a guide on how to install either refer to :ref:`pythas_installation`.
Both can be detected automatically if they are already installed. In case *Stack* is installed, it will always be used as the default.

Basic Usage
-----------

Usage is as easy as importing any Python modules. If we have an ``example/Example.hs`` file containing a Hello World function ``hello :: IO ()`` which prints "Hello from Haskell" we would import and use it like that:

.. code-block:: python

    >>> import pythas
    >>> import example.example as e
    >>> e.hello()
    Hello from Haskell!

In the :ref:`pythas_usage` chapter you can find more details on how to use *Pythas*. More practical examples are collected in a separate repository `Pythas-Examples <https://github.com/pinselimo/Pythas-Examples>`_.

Contents
========

.. toctree::
   :maxdepth: 2

   installation
   usage
   details
   misc
   reference
   reference-dev
   🐈 GitHub <https://github.com/pinselimo/Pythas>
   Pythas-Examples <https://github.com/pinselimo/Pythas-Examples>


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

