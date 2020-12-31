.. _pythas_usage:

Usage
=====

With *Pythas* installed you have multiple options of compiling and importing Haskell source code into Python. In all cases ``import pythas`` is the precondition to any consequent use of Haskell source.

Note that the first time you use ``import pythas`` its parts implemented in Haskell are compiled, which impacts import time. Following usages will not show this behaviour anymore. Only updates of *Pythas* which alter parts of its Haskell source will trigger recompilation.

Static Haskell Modules
----------------------

For static Haskell source files *Pythas* aims to provide a pythonic import behaviour. Consider you execute ``python`` in a directory which contains a Haskell module ``Example.hs`` in a ``examples`` subdirectory::

    .
    └─── examples
        |     Example.hs

Then you can import this module simply by typing:

.. code-block:: python

    >>> import pythas
    >>> import examples.example as example

The ``example`` module and the function it contains can now be accessed from python just like any usual python package. Note how we use a capitalized module name in Haskell and a lower case one in Python. This way, module naming schemes stay consistent with regard to both languages. Another tweak is, that the ``examples`` directory does not need a ``__init__.py`` file to be considered in the module lookup. Instead, *Pythas* will trust your knowledge about the file path and search accordingly. 

After the import, the module presents itself to the user just like a Python module would. Given the following code in ``Example.hs``:

.. code-block:: haskell

    increment :: Int -> Int
    increment = (1+)

then this means you can call it from Python as you expect:

.. code-block:: python

    >>> example.increment(1)
    2

Inline Haskell Modules
----------------------

Inspired by *pyCUDA* the ``SourceModule`` - Object was added as another option for compiling Haskell source directly from a Python context.

.. code-block:: python

    >>> from pythas import SourceModule
    >>> m = SourceModule('''
            increment :: Int -> Int
            increment = (1+)
            ''')
    >>> m.increment(1)
    2

Limitations
-----------

In both cases some limitations exist on which Haskell functions and constants can and will be imported. Most notably, type declarations are paramount for the imports as *Pythas* does not do its own type inference. All basic Haskell types are supported, including nested lists and tuples and strings.

Invalid functions or constants will not be available from the Python context. However, they will not trigger any errors. Thus, they can be used within the Haskell context without risk.

Constants vs IO Constants
-------------------------

Consider two type annotations in Haskell:

.. code-block:: haskell

    a :: Int
    b :: IO Int

Interfacing from Python through *Pythas* these constants/variables (let's just not go down that rabbit hole right now) will be available like so:

.. code-block:: python

    >>> m.a
    63
    >>> m.b
    <pythas.utils.PythasFunc object at 0x....>
    >>> m.b()
    63

The second name ``b`` needs to be called in order to expose its value. This is actually somewhat convenient, as it exposes part of Haskells strict notion on purity in Python. However, it gets fuzzy when we try to use nested data types (i.e. anything that needs a pointer - Lists, Tuples, Strings & Custom Types). *Pythas* will need to wrap these using memory operations. Thus, even pure code is lifted into the IO monad for data transfer. So, if we take ``a`` and ``b`` instead to be:

.. code-block:: haskell

    a :: [Int]
    b :: IO [Int]

We will end up with the following on Python's side:


.. code-block:: python

    <pythas.utils.PythasFunc object at 0x....>
    >>> m.a
    <pythas.utils.PythasFunc object at 0x....>
    >>> m.a()
    [1,2,3]
    >>> m.b()
    [1,2,3]

The call signature of ``b`` doesn't change, but ``a`` requires unwrapping now and it shows. In effect, you lose the visible difference the IO monad would cause on Python's side in the first example.

Note that the purity of your code itself does not suffer under this restriction! It just makes the call syntax a little weird.

