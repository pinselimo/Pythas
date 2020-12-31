.. _pythas_details :

How does this magic work?
-------------------------

As has been layed out, the Haskell source code is still compiled using the well established ``Glorious Glasgow Haskell Compiler`` (GHC). So how is the compiled source accessed from Python?

Both Python and Haskell provide Foreign Function Interfaces to facilitate communication across language boundaries. However, in both languages this is a tedious process. ``Pythas`` aims to automate this on both sides.

The ease of use paradigm that ``Pythas`` development is subject to implied a minimal set of requirements. Therefore, the users themselves should not be exposed to ``Template Haskell`` or any foreign function interfaces.

The process comprises 4 major steps:

Parsing
^^^^^^^

Yes, ``Pythas`` ships its own parser. This is the main reason you'll need to wait a little longer for the first import. As parsing is famously convenient in Haskell, most of it is implemented using ``Parsec``.

A second minimal parser is contained in the ``pythas.parser`` package. This mini-parser will only parse ``foreign export ccall`` statements and derive the type conversion necessary on Python's side.

Type conversion
^^^^^^^^^^^^^^^

Haskell and Python types are not directly compatible. In both languages lists are a heavily used concept these represent fundamentally different types in either idiom. Python's dynamic typing doesn't interface well with Haskell's strict and (as for the FFI) static type system.

Moreover, there are no guarantees on which exact representation on memory will be used and it can depend on the backend. Both language's FFIs provide rich sets of C data types which solve this problem.
``Pythas`` will wrap all compatible Haskell functions to accept and provide interfacing compatible types. This type conversion type checks with GHC at compile time!

On the Python side it will then pack the data in the pythonic equivalent to your Haskell data type and provide it to you.

Code generation
^^^^^^^^^^^^^^^

To provide ``GHC`` with some source to compile a temporary Haskell module is generated. All files additionally created will be removed directly after the compiled module is imported in Python.
Exports contained in the original module will also be included in the final import and are not affected by the generated code.

Compilation and import
^^^^^^^^^^^^^^^^^^^^^^

The module is ultimately compiled into a shared library stored in a temporary file, This is the actual binary imported into Python. In Python it is again wrapped to provide a pythonic interface just as you would expect it from any other Python import.

Voilá, you can use Haskell source from Python!

.. _pythas_interface :

Interfacing with GHC
--------------------

Regardless of the build tools utilized, a minimal interface to ``GHC`` is provided. The compiler is wrapped as ``Compiler`` object. Internally another abstraction step is taken with the ``GHC`` object stored as ``ghc`` attribute of the ``Compiler`` instance.

Compile time flags
^^^^^^^^^^^^^^^^^^

Flags for compilation can be set using the ``add_flag`` method. Consequently, the ``flags`` attribute of the ``Compiler`` instance contains a tuple with the set flags. Note that flags used for the general functionality of ``Pythas`` are not exposed here. Thus, any flag contained within the ``flags`` attribute can be removed using ``remove_flag`` method.

Optimisation flag
^^^^^^^^^^^^^^^^^

The default value for the Optimisation flag is already set to ``-O2``. However, if for some reason you want to change this value you can do using the ``GHC`` instance so e.g.: ``compiler.ghc.Optimisation = 1`` will set ``-O1`` at compile time.

