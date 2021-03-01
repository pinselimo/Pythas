from .context import pythas

from hypothesis import strategies, given

strings = strategies.text()
tuples = strategies.tuples


def test_dynamic_dir():
    import test.hs.testcases as t

    init_dir = set(dir(t))
    t.something = 63
    new_dir = set(dir(t))
    assert new_dir - init_dir == {"something"}


def test_sourcemodules():
    m = pythas.SourceModule(
        """
            i :: Int
            i = 63

            f :: [(String, String)] -> Int
            f = length
            """,
        compiler=pythas.compiler,
    )
    assert m.i == 63
    assert m.f([("a", "b"), ("c", "d")]) == 2


def test_stack_switch():
    pythas.compiler.stack_usage = False
    import test.hs.testcases as t

    assert t.constantInt == 63


@given(tuples(strings, strings), strings)
def test_compilerflags(t, s):
    base_flags = pythas.compiler.flags
    pythas.compiler.add_flag(t)
    pythas.compiler.add_flag(s)
    assert pythas.compiler.flags == base_flags + t + (s,)
    pythas.compiler.remove_flag(t)
    assert pythas.compiler.flags == base_flags + (s,)
    pythas.compiler.remove_flag(s)
    assert pythas.compiler.flags == base_flags
