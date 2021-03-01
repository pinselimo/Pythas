.. _pythas_details :

Advanced Topics
===============

This chapter covers details for the interested reader. It is a great first read if you want to delve into *Pythas*' source and contribute. As is reading through the tests in the ``test`` directory.
Be sure to first read :ref:`pythas_installation` and :ref:`pythas_usage` before taking in this deep dive.

How does this magic work?
-------------------------

As has been layed out, the Haskell source code is still compiled using the well established *glorious Glasgow Haskell Compiler* (GHC). So how is the compiled source accessed from Python?

Both Python and Haskell provide Foreign Function Interfaces (FFI) to facilitate communication across language boundaries. However, in both languages this is a tedious process. *Pythas* aims to automate it on both sides.
The ease of use paradigm that *Pythas* development is subject to implied a minimal set of requirements; i.e. the users themselves should not be exposed to *Template Haskell*, "Cabal hell" or any foreign function interfacing.

The process of importing a Haskell module using *Pythas* comprises 4 major steps:

Parsing
^^^^^^^

Yes, *Pythas* ships its own parser. This is the main reason you'll need to wait a little longer for the first import. As parsing is famously convenient in Haskell, most of it is implemented using *Parsec*. This parser can be found in the `Pythas-FFI <https://github.com/pinselimo/Pythas-FFI>`_ repository.

A second minimal parser is contained in the ``pythas.parser`` package. This mini-parser will only parse ``foreign export ccall`` statements and derive the type conversion necessary on Python's side.

Type conversion
^^^^^^^^^^^^^^^

Haskell and Python types are not directly compatible. In both languages lists are a heavily used concept. However, these represent fundamentally different types in either idiom. Python's dynamic typing doesn't interface well with Haskell's strict and (as for the FFI) static type system.

Moreover, for some types there are no guarantees which exact representation on memory will be used. This can depend on the backend. Both language's FFIs provide rich sets of C data types which solve this problem.
*Pythas* will wrap all compatible Haskell functions to accept and provide interfacing compatible types. This type conversion type checks with GHC at compile time!
On the Python side it will then pack the data in the pythonic equivalent to your Haskell data type and provide it to you.

Code generation
^^^^^^^^^^^^^^^

To provide *GHC* with some static source code to compile, a temporary Haskell module is generated. All files additionally created will be removed directly after the compiled module is imported in Python.
Haskell FFI exports contained in the original module will also be imported and are not affected by the generated code.

Additionally to the wrapped functions, *Pythas* will also add specific functions for freeing any memory allocated by the Haskell runtime.

Compilation and import
^^^^^^^^^^^^^^^^^^^^^^

The module is ultimately compiled into a shared library stored in a temporary file. This is the actual binary imported into Python. Within Python it is again wrapped to provide a pythonic interface just as you would expect it from any other Python import.
*Pythas* will also add automated calls to the functions freeing memory described above.

Voilá, you can use Haskell source from Python!

.. _pythas_interface :

Interfacing with GHC
--------------------

Regardless of the build tools utilized, a minimal interface to *GHC* is provided. The compiler is wrapped as ``Compiler`` object. Internally another abstraction step is taken with the ``GHC`` class which handles specifics of the actual compiler.

Compile time flags
^^^^^^^^^^^^^^^^^^

Flags for compilation can be set using the ``add_flag`` method. Consequently, the ``flags`` attribute of the ``Compiler`` instance contains a tuple with the set flags. Note that flags used for the general functionality of *Pythas* are not exposed here. Thus, any flag contained within the ``flags`` attribute can be removed using ``remove_flag`` method without risk.

The default value for the optimisation flag is already set to ``-O2``. However, if for some reason you want to change this value or remove it, you can do so by using ``compiler.remove_flag('-O2')``.

Notes on faster execution times
-------------------------------

Neither quick compilation nor execution are main objectives of *Pythas* development at this stage. In contrast, the aim is to emphasize Haskell's benefits and provide easier access to them for Python users. There are, however, some tricks that can speed up your usage of Haskell code through *Pythas* significantly:

Whenever the interface has to hand over a list, a new ``struct`` containing a C array and an integer with its length is created. This happens both in the direcetion Haskell - Python as well as in Python - Haskell direction. Even in cases where a list is handed in both directions, the pointer/array will not be re-used!
Thus, to save execution time, consider moving ``map``, ``foldr`` or similar calls into the Haskell source.

Similarly, pointers to the structs created for the transfer of tuples are not reused.

