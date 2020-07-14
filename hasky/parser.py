
from collections import namedtuple

ParseInfo = namedtuple('ParseInfo',['name','dir','exported_mod','exported_ffi','func_infos'])
FuncInfo = namedtuple('FuncInfo',['name','argtypes','restype','constructors','reconstructor','htype'])
