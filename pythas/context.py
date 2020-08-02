from subprocess import run
from abc import ABCMeta, abstractmethod, abstractproperty

from .utils import flatten

class Compiler(metaclass=ABCMeta):
    def __init__(self):
        self._custom_flags = Flags()

    @property
    def custom_flags(self):
        return self._custom_flags

    @abstractmethod
    def compile(self, filename, libname, redirect=False):
        pass

class Context:
    def __init__(self, compiler):
        self.__compiler = compiler

    @property
    def compiler(self):
        return self.__compiler

    def compile(self, filename, libname, redirect=False):
        self.__compiler.compile(filename, libname, redirect)

class Flags:
    def __init__(self):
        self._flags = list()

    def __call__(self):
        return tuple(flatten(self._flags))

    def add_flag(self, flag):
        if flag not in self._flags:
            self._flags.append(flag)

    def remove_flag(self, flag):
        if flag in self._flags:
            self._flags.remove(flag)

