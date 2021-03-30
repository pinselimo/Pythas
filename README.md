# Pythas [![Python package](https://github.com/pinselimo/Pythas/actions/workflows/python-package.yml/badge.svg)](https://github.com/pinselimo/Pythas/actions/workflows/python-package.yml) [![Documentation Status](https://readthedocs.org/projects/pythas/badge/?version=latest)](https://pythas.readthedocs.io/en/latest/?badge=latest)

Import Haskell modules as if they were Python modules. If an imported name does not exist as Python module/package, Pythas will traverse the specified subdirectory below your ```cwd``` to look for a matching Haskell file. If one is found it is imported just as if it was a Python module.

If you have a file ```Example.hs``` like in the ```example``` directory, it will be imported and can be used like such:

~~~python
>>>import pythas
>>>import example.example as e
>>>e.hello()
Hello from Haskell!
~~~

You can also just ```from * import```. Try:

~~~python
>>>from example.example import multisin
>>>from math import pi
>>>multisin(2,pi)
2.4492935982947064e-16
~~~

and you'll see: It doesn't stop at invoking side-effects.

## Sequences

Python ```Sequences``` can be passed as linked lists or as arrays. Depending on which flavour of programming you want to embrace. Currently for speed and space reasons arrays are used by the backend, but it should be no problem to change that for those hardcore FP nerds stuck in ```Haskell```-land.
Try things like:

~~~python
>>>from example.example import mapQuarter
>>>mapQuarter(range(1000,5000,1000))
[250.0, 500.0, 750.0, 1000.0]
~~~

While in Haskell lists **have** to be used, in Python any kind of sequence can be handed over. Needless to say, varying types won't be supported.

## Tuples

You can use tuples to pack results of different types into a single one. It is no problem to nest them and lists or vice versa. Checkout ```test.hs.Test.hs``` to see what Pythas is (successfully) tested for.

~~~python
>>>from example.example import tuple, hsnd
>>>tuple()
(1,"a")
>>>hsnd((1,2))
2
~~~

## Requirements

Please make sure that ```GHC``` is located in your ```$PATH```. ```Pythas``` requires at least ```GHC``` version ```8.0.2```. It defaults to using ```stack ghc```. To check if your ```GHC``` version is compatible run:

~~~bash
$ stack ghc -- --version
~~~

or without ```stack``` installed:

~~~bash
$ ghc --version
~~~

```Pythas``` is written with compatibility and ease of use in mind. All libraries used in the ```Haskell``` backend are contained in the standard installation of GHC. No requirements exist on the ```Python```ic side of life.

## Install

```Pythas``` can be installed using pip. Run the following command to install and compile it at once:

~~~sh
$ pip install pythas && python -c "import pythas"
~~~

or from source:

~~~sh
$ git clone https://github.com/pinselimo/Pythas.git && cd Pythas
$ pip install . && python -c "import pythas"
~~~

## Constraints

Only Python versions 3.7 and up are supported. Unfortunately, only [PEP 562] introduced ```__getattr__``` for modules. This renders the level of abstraction ```Pythas``` aims for impossible on lower Python versions.

Only functions having their type declared will be imported. You can handle the export of the function yourself by adding a ```foreign export ccall``` for the function, otherwise ```Pythas``` will do that for you. To exclude a function just omit the functions type. Functions of types that are not supported won't get exported either.

All Haskell constants in the IO monad are imported as functions. Due to lists being turned into ```CArray```s even constant lists must be called like a function without arguments:

~~~python
>>>from example.example import someConstant, haskellList
>>>someConstant
63
>>>haskellList()
[63]
~~~

The same is true for tuples which are turned into or ```CTuple{x}```s.

 ```Pythas``` enforces the file naming scheme of Haskell for  ```.hs``` files as does the ```GHC```! This is primarily due to  ```GHC``` failing to find the imported module at compile time. Thus, we fail early and raise a ```ModuleNotFoundError```.

## Docs

+ Plentiful documentation is available at [ReadTheDocs.io](https://pythas.readthedocs.io/en/latest/)
+ Practical examples can be found in the [sister repository](https://github.com/pinselimo/Pythas-Examples)

## Contributing

Meaningful contributions are always welcome, please refer to ```CONTRIBUTING.md``` for details.

## Testing

The sub-repositories Pythas incorporates all ship their own testing facilities. Testing of the ```pythas``` package itself therefore focuses on the main functionality and user interface. It incorporates some unit tests and property based testing mostly focusing on ensuring the interface does not alter any data. To execute the tests in bulk just move to the project root and execute:

~~~bash
$ pytest
~~~

## License

The Software in this repository is licensed under the LGPLv3 License.
See COPYING.LESSER file for details.

    Pythas  Copyright (C) 2020  Simon Plakolb
    This program comes with ABSOLUTELY NO WARRANTY
    This is free software, and you are welcome to redistribute it
    under certain conditions; see COPYING and COPYING.LESSER.

