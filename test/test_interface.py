from .context import pythas
import test.hs.testcases as t

def test_dynamic_dir():
    init_dir = set(dir(t))
    t.something = 63
    new_dir  = set(dir(t))
    assert new_dir - init_dir == {'something'}

