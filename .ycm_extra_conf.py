def FlagsForFile(filename, **kwargs):
    flags = ['-Wall', '-Wextra', '-Werror']

    if filename in ['test.c', 'module.c']:
        flags.extend("-lX11" "-lxcb")
        pass

    return {
        'flags': flags
    }
