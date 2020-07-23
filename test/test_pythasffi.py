from hypothesis import given
import hypothesis.strategies as strat
from numpy import sin

from .context import pythas

types = pythas.types
utils = pythas.utils

c_ints = strat.integers(min_value=-46340,max_value=46340)
c_integers = strat.integers()
c_floats = strat.floats(allow_nan=False, width=32, allow_infinity=False)
c_doubles = strat.floats(allow_nan=False, allow_infinity=False)
# ctypes stopped supporting embedded NULL characters
c_strings = strat.text(alphabet=strat.characters(blacklist_characters='\0'))

list_integers = strat.lists(c_integers)
list_strings  = strat.lists(c_strings)
list_str_nst3 = strat.lists(strat.lists(list_strings))



import test.hs.test as t

def test_constantInt():
    assert t.constantInt == 63

def test_constantString():
    assert t.constantString() == "Hello from Haskell!"

def test_constantList():
    assert t.constantList() == [63]

def test_constantTuple():
    assert t.constantTuple() == (0.63, "Haskell yay")

def test_testAlignment():
    assert t.testAlignment() == [(0.1, 63, b'a')]*10

def test_constantTriple():
    i,d,f = t.constantTriple()
    assert i == 63
    assert d == 0.63
    assert f > 0.62 and f < 0.64

def test_constantQuadruple():
    assert t.constantQuadruple() == (63, 0.1, b'?', b'a')

### SIDE EFFECTS CURRENTLY NOT TESTED ###

# Testing of operations

@given(c_ints)
def test_pureOperationInt(i):
    assert t.pureOperationInt(i) == i*i

'''
The inaccuracy of floats can hardly be asserted in Python
@given(c_floats, c_floats)
def test_pureOperationFloat(a,b):
    if a <= 0:
        assert True
    else:
        try:
            res = t.pureOperationFloat(a,b)
            assert res >= (a**b)-2 and res <= (a**b)+2
        except OverflowError:
            assert True
'''
@given(c_strings)
def test_pureOperationString(s):
    assert t.pureOperationString(s) == ''.join(filter(lambda x:x!='a',s))

@given(c_ints, c_doubles)
def test_pureOperationMixed(i,d):
    assert t.pureOperationMixed(i,d) == i*sin(d)

@given(list_integers)
def test_listOfIntegers(l):
    assert t.listOfInteger(l) == len(l)

@given(strat.lists(c_ints))
def test_listMixed(l):
    assert t.listMixed(l) == list(map(lambda x:x*0.25,l))

@given(list_str_nst3)
def test_listNested(lll):
    res = ""
    try:
        res = lll[0][0][0]
    except IndexError:
        pass
    assert t.listNested(lll) == res

@given(c_strings, c_ints)
def test_listOfTuples(s,i):
    assert t.listOfTuples(s,i) == [(s,i)]

@given(c_strings, c_ints)
def test_listOfTuplesNested(s,i):
    assert t.listOfTuplesNested(s,i) == [[(i,s)]]

@given(list_strings)
def test_listOfTuplesWithList(ss):
    assert t.listOfTuplesWithList(ss) == [[([ss]*63,[63])]]

@given(c_ints)
def test_tupleWithList(i):
    assert t.tupleWithList(i) == (['Haskell']*i,[63]*i)

@given(c_ints, c_strings)
def test_tupleWithNestedList(i,s):
    assert t.tupleWithNestedList(i,s) == ([[s]*i],[[i]])

@given(c_strings, c_strings)
def test_tupleWithListOfTuples(a,b):
    assert t.tupleWithListOfTuples(a,b) == (63*[(a,b)], [63])
