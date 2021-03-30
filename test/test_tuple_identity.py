from hypothesis import given
from hypothesis.strategies import tuples, lists

from .context import pythas
from .t_types import *

types = pythas.types
utils = pythas.utils

import test.hs.testtupleidentity as hst


@given(tuples(c_ints, c_ints))
def test_intInt(l):
    assert hst.intInt(l) == l


@given(tuples(c_doubles, c_doubles))
def test_doubleDouble(l):
    assert hst.doubleDouble(l) == l


@given(tuples(c_ints, c_chars, c_doubles))
def test_intCharDouble(l):
    assert hst.intCharDouble(l) == l


@given(tuples(c_doubles, c_floats, c_chars, c_doubles))
def test_doubleFloatCharDouble(l):
    assert hst.doubleFloatCharDouble(l) == l


# Lists and Nested


@given(tuples(c_ints, lists(c_strings)))
def test_intListStrings(l):
    assert hst.intListStrings(l) == l


@given(tuples(c_strings, lists(tuples(c_ints, c_chars))))
def test_stringListTupleIntChar(l):
    assert hst.stringListTupleIntChar(l) == l


@given(tuples(tuples(c_ints, c_doubles), tuples(c_ints, c_doubles)))
def test_nestedIntDouble(l):
    assert hst.nestedIntDouble(l) == l
