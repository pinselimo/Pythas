from .context import pythas
import test.hs.testcases as t

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

