import hypothesis.strategies as strat

c_chars = strat.binary(min_size=1, max_size=1)
c_ints = strat.integers(min_value=-46340, max_value=46340)
c_integers = strat.integers()
c_floats = strat.floats(allow_nan=False, width=32, allow_infinity=False)
c_doubles = strat.floats(allow_nan=False, allow_infinity=False)
# ctypes stopped supporting embedded NULL characters
c_strings = strat.text(alphabet=strat.characters(blacklist_characters="\0"))
