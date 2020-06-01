# Hasky

Import Haskell modules as if they were Python modules. If an imported name does not exist as Python module/package, Hasky will traverse the specified subdirectory below your ```cwd``` to look for a matching Haskell file. If one is found it is imported just as if it was a Python module.

If you have a file ```Example.hs``` like in the ```example``` directory, it will be imported and can be used like such:

~~~python
>>>import hasky
>>>import example.example as e
>>>e.hello()
Hello from Haskell
~~~

You can also just ```from * import```. Try:

~~~python
>>>from example.example import multisin
>>>from math import pi
>>>multisin(2,pi)
~~~

and you*ll see: It doesn't stop at invoking side-effects.

## Sequences

Python ```Sequences``` can be passed as linked lists or as arrays. Depending on which flavour of programming you want to embrace. Change the ```USE_LISTS``` flag in ```hasky.haskell.parse_type``` to switch in between the two. Then try things like:

~~~python
>>>from example.example import mapQuarter
>>>mapQuarter(range(1000,5000,1000))
~~~

## Install

 ```Hasky``` can be installed using pip:

 ~~~sh
 $ pip install .
 ~~~

## Constraints

Only functions having their type declared will be imported. You can handle the export of the function yourself by adding a ```foreign export ccall``` for the function, otherwise ```Hasky``` will do that for you. To exclude a function from being automatically exported by ```Hasky``` add a comment ```--(HASKY-EXCLUDE <function-name>``` where ```<function-name>``` is the name of the function to be excluded before the function declaration; Or just ommit the functions type.

All Haskell constants in the IO monad are imported as functions. Due to lists being turned into ```CArray```s or ```CList```s, also constant lists must be called like a function without arguments:

~~~python
>>>from example.example import someConstant, haskellList
>>>someConstant
63
>>>haskellList()
[63]
~~~

 ```Hasky``` enforces the file naming scheme of Haskell for  ```.hs``` files as does the ```GHC```! This is primarily due to  ```GHC``` failing to find the imported module at compile time. Thus, we fail early and raise a ```ModuleNotFoundError```.

## License

The Software in this repository is licensed under the LGPLv3 License.
See COPYING.LESSER file for details.

    Hasky  Copyright (C) 2020  Simon Plakolb
    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; see COPYING and COPYING.LESSER.
