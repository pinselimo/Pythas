.. _pythas_usage:

Usage
=====

With ``Pythas`` installed you have multiple options of compiling and importing Haskell source code into Python. In all cases ``import pythas`` is the precondition to any consequent use of Haskell source.

Note that the first time you use ``import pythas`` its parts implemented in Haskell are compiled, which impacts import time. Following usages will not show this behaviour anymore. Only updates of ``Pythas`` which alter parts of its Haskell source will trigger recompilation.

Existing Haskell Modules
------------------------

For existing Haskell source code ``Pythas`` aims to provide a pythonic import behaviour. Consider you execute ``python`` in a directory which contains a Haskell module ``Example.hs`` in a ``examples`` subdirectory::

    .
    └─── examples
        |     Example.hs

Then you can import this module simply by typing:

.. code-block:: python
    >>> import pythas
    >>> import examples.example as example

 The ``example`` module and the function it contains can now be accessed from python just like any usual python package. Given the following code in ``Example.hs``:

.. code-block:: haskell
    increment :: Int -> Int
    increment = (1+)

then this means you can call it from Python as you expect:

..code-block:: python
    >>> example.increment(1)
    2

Some limitations exist on the 

Inline Haskell Modules
----------------------

Inspired by ``pyCUDA`` the ``SourceModule`` - Object was added as another option for compiling Haskell source directly from a Python context.

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

In both cases there are some limitations upon the Haskell code which is compilable.

  * Type annotations need to be supplied
  * Only Haskell native types can be used

Invalid functions or constants will not be available from the Python context. However, they will not trigger any errors. Thus, they can be used within the Haskell context without risk.

Speed
-----

Neither quick compilation nor execution are main objectives of ``Pythas`` development at this stage. Nonetheless, the optimization flag is set to its maximum level of 2. For more details refer to pythas_interface_.

