
from collections import namedtuple

ParseInfo = namedtuple('ParseInfo',['name','dir','exported_mod','exported_ffi','excluded','func_infos'])
FuncInfo = namedtuple('FuncInfo',['name','functype','exporttype','argtypes','restype','constructors','reconstructor','destructor', 'destr_type','native_from_c','native_to_c'])