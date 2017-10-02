def FlagsForFile(filename, **kwargs):
    flags = ['-Wall', '-Wextra', '-Werror']

    if filename == "test.c":
        flags.extend("-Ilibevdev-1.0/libevdev" "-levdev")
        pass

    return {
        'flags': flags
    }
