class Context:
    def __init__(self, cmd : tuple, base_args : tuple, more_args : tuple):
        self._cmd = cmd
        self._base_args = base_args
        self._more_args = more_args

    def add_arg(self, arg):
        self._more_args.append(arg)

    def remove_arg(self, arg):
        self._more_args = tuple(a for a in self._more_args if a not arg)

    @property
    def args(self):
        return self._base_args + self._more_args

    def compile(self):
