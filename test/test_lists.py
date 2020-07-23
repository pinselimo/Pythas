from hypothesis import given
from hypothesis.strategies import integers, floats, binary, text, characters, tuples, lists

from .context import pythas

types = pythas.types
utils = pythas.utils

c_ints = integers(min_value=-46340,max_value=46340)
c_floats = floats(allow_nan=False, width=32, allow_infinity=False)
c_doubles = floats(allow_nan=False, allow_infinity=False)
c_chars = binary(min_size=1,max_size=1)
# ctypes stopped supporting embedded NULL characters
c_strings = text(alphabet=characters(blacklist_characters='\0'))

import test.hs.testlists as hst

@given(lists(c_ints))
def test_listInt(l):
    assert hst.listInt(l) == l

@given(lists(c_doubles))
def test_listDouble(l):
    assert hst.listDouble(l) == l

@given(lists(c_chars))
def test_listDouble(l):
    assert hst.listChar(l) == l

@given(lists(c_strings))
def test_listString(l):
    assert hst.listString(l) == l

# Nested

@given(lists(lists(c_ints)))
def test_listListInt(l):
    assert hst.listListInt(l) == l

@given(lists(lists(c_strings)))
def test_listListString(l):
    assert hst.listListString(l) == l

# @given(lists(lists(tuples(c_strings,c_strings))))
# def test_listListTupleStringString(l):
#     assert hst.listListTupleStringString(l) == l

@given(lists(lists(lists(c_strings))))
def test_listListListString(l):
    assert hst.listListListString(l) == l

