from hypothesis import given
from hypothesis.strategies import tuples, lists

from .context import pythas
from .t_types import *

types = pythas.types
utils = pythas.utils

# Haskell import
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


@given(lists(lists(tuples(c_strings, c_strings))))
def test_listListTupleStringString(l):
    assert hst.listListTupleStringString(l) == l


@given(lists(lists(lists(c_strings))))
def test_listListListString(l):
    assert hst.listListListString(l) == l
