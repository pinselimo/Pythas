from subprocess import run

class Context:
    def __init__(self, cmd : tuple, base_args : tuple, more_args : tuple):
        self._cmd = cmd
        self._base_args = base_args
        self._more_args = more_args

    def add_arg(self, arg: str):
        self._more_args.append(arg)

    def remove_arg(self, arg : str):
        self._more_args = tuple(a for a in self._more_args if a not arg)

    @property
    def args(self) -> tuple:
        return self._base_args + self._more_args

    def build_cmd(self, filename : str, libname : str, filedir : str, platform : str) -> tuple:
        return self._cmd + self._base_args + self._more_args

    def compile(self, filename : str, info : ParseInfo):
        filedir = parse_info.dir
        name = parse_info.name.lower()
        libname = os.path.join(filedir,'lib'+name)
        if platform.startswith('linux'):
            libname += '.so'
        elif platform.startswith('win32'):
            libname += '.dll'
        cmd = ghc_compile_cmd(filename, libname, filedir, platform)
        print('Compiling with: {}'.format(cmd[0]))
        run(cmd)
        return libname
