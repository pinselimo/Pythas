from hypothesis import given
from hypothesis.strategies import tuples, lists

from .context import pythas
from .t_types import *

types = pythas.types
utils = pythas.utils

import test.hs.testtuples as hst

# 2 fields
@given(c_ints, c_doubles)
def test_int32Double(a, b):
    assert hst.int32Double(a, b) == (a, b)


@given(c_doubles, c_ints)
def test_doubleInt32(a, b):
    assert hst.doubleInt32(a, b) == (a, b)


@given(c_chars, c_ints)
def test_charInt32(a, b):
    assert hst.charInt32(a, b) == (a, b)


@given(c_ints, c_chars)
def test_int32Char(a, b):
    assert hst.int32Char(a, b) == (a, b)


## Nested
@given(lists(c_ints), c_strings)
def test_list2A(a, b):
    assert hst.list2A(a, b) == (a, b)


@given(c_ints, lists(c_strings))
def test_list2B(a, b):
    assert hst.list2B(a, b) == (a, b)


# 3 fields
@given(c_ints, c_doubles, c_chars)
def test_int32DoubleChar(a, b, c):
    assert hst.int32DoubleChar(a, b, c) == (a, b, c)


@given(c_doubles, c_ints, c_chars)
def test_doubleInt32Char(a, b, c):
    assert hst.doubleInt32Char(a, b, c) == (a, b, c)


@given(c_chars, c_ints, c_doubles)
def test_charInt32Double(a, b, c):
    assert hst.charInt32Double(a, b, c) == (a, b, c)


@given(c_ints, c_chars, c_chars)
def test_int32CharChar(a, b, c):
    assert hst.int32CharChar(a, b, c) == (a, b, c)


## Nested
@given(lists(c_ints), c_strings, c_floats)
def test_list3A(a, b, c):
    assert hst.list3A(a, b, c) == (a, b, c)


@given(c_ints, lists(c_strings), c_floats)
def test_list3B(a, b, c):
    assert hst.list3B(a, b, c) == (a, b, c)


@given(c_ints, c_strings, lists(c_floats))
def test_list3C(a, b, c):
    assert hst.list3C(a, b, c) == (a, b, c)


# 4 fields
@given(c_ints, c_doubles, c_doubles, c_chars)
def test_int32DoubleDoubleChar(a, b, c, d):
    assert hst.int32DoubleDoubleChar(a, b, c, d) == (a, b, c, d)


@given(c_doubles, c_ints, c_chars, c_chars)
def test_doubleInt32CharChar(a, b, c, d):
    assert hst.doubleInt32CharChar(a, b, c, d) == (a, b, c, d)


@given(c_chars, c_ints, c_doubles, c_chars)
def test_charInt32DoubleChar(a, b, c, d):
    assert hst.charInt32DoubleChar(a, b, c, d) == (a, b, c, d)


@given(c_ints, c_chars, c_chars, c_chars)
def test_int32CharCharChar(a, b, c, d):
    assert hst.int32CharCharChar(a, b, c, d) == (a, b, c, d)


## Nested
@given(lists(c_ints), c_strings, c_floats, c_doubles)
def test_list4A(a, b, c, d):
    assert hst.list4A(a, b, c, d) == (a, b, c, d)


@given(c_ints, lists(c_strings), c_floats, c_doubles)
def test_list4B(a, b, c, d):
    assert hst.list4B(a, b, c, d) == (a, b, c, d)


@given(c_ints, c_strings, lists(c_floats), c_doubles)
def test_list4C(a, b, c, d):
    assert hst.list4C(a, b, c, d) == (a, b, c, d)


@given(c_ints, c_strings, c_floats, lists(c_doubles))
def test_list4C(a, b, c, d):
    assert hst.list4D(a, b, c, d) == (a, b, c, d)
