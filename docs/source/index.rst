.. Pythas documentation master file, created by
   sphinx-quickstart on Mon Aug 17 19:40:45 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Pythas' documentation!
==================================

With *Pythas* it is possible to import Haskell modules into Python just as if they were native modules. It automatically creates the FFI exports including all relevant type conversions, compiles and binds your Haskell code to the Python at runtime. It was designed with ease of use in mind and supports a pythonic, intuitive interface.

Dependencies
------------

One development goal for *Pythas* was to keep users out of cabal hell. Thus, the pacakage includes all Haskell and C source files needed. To compile its own source and other Haskell modules *Pythas* needs either *Stack* or a plain *GHC* installation.
f either of those is installed they can be detected automatically. If *Stack* is installed, it will always be used as default.
A more detailed explanation can be found in chapter :ref:`pythas_installation`.

Basic Usage
-----------

With an ``example/Example.hs`` file containing a Hello World function ``hello :: IO ()``

.. code-block:: python

    >>> import pythas
    >>> import example.example as e
    >>> e.hello()
    Hello from Haskell!

In the :ref:`pythas_usage` chapter you can find more details on using *Pythas*. More practical examples are collected in a separate repository `Pythas-Examples <https://github.com/pinselimo/Pythas-Examples>`_.

Contents
========

.. toctree::
   :maxdepth: 2

   installation
   usage
   details
   misc
   reference-dev
   🐈 GitHub <https://github.com/pinselimo/Pythas>
   Pythas-Examples <https://github.com/pinselimo/Pythas-Examples>


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

