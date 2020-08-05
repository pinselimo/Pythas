from .context import pythas
import test.hs.testcases as t

from hypothesis import strategies, given

strings = strategies.text()
tuples  = strategies.tuples

def test_dynamic_dir():
    init_dir = set(dir(t))
    t.something = 63
    new_dir  = set(dir(t))
    assert new_dir - init_dir == {'something'}

def test_sourcemodules():
    m = pythas.SourceModule('''
            i :: Int
            i = 63

            f :: [(String, String)] -> Int
            f = length
            ''')
    assert m.i == 63
    assert m.f([('a','b'),('c','d')]) == 2

@given(tuples(strings,strings), strings)
def test_compilerflags(t,s):
    pythas.compiler.add_flag(t)
    pythas.compiler.add_flag(s)
    assert pythas.compiler.flags == t + (s,)
    pythas.compiler.remove_flag(t)
    assert pythas.compiler.flags == (s,)
    pythas.compiler.remove_flag(s)
    assert pythas.compiler.flags == ()

