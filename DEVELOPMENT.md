Great to have you here! Here are a few ways you can help out with [Nelson](https://github.com/nelson-lang/nelson).

# Where should I start?

## Your first commits

If you are interested in contributing to Nelson, that's awesome! We love your help.
If you have any questions after reading this page, please feel free to contact [Nelson](https://github.com/nelson-lang). I will be happy to provide help working through your first bug fix or thinking through the problem you are trying to resolve.

## How you can help

We track [bugs and features](https://github.com/nelson-lang/nelson/issues) so that anyone who wants to help can start with something that's not too overwhelming.
We can absolutely use your help, no matter what level of programming skill you have at the moment.

This guide covers the repository layout, the entry points for building, and
the project-wide code conventions. It complements:

- [CONTRIBUTING.md](CONTRIBUTING.md): contribution process and commit style.
- [BUILDING.md](BUILDING.md) and the per-platform BUILDING files: dependencies and first build.

# Repository map

`modules/` contains about 100 self-contained modules. Each module follows the
same canonical layout:

```
modules/<module>/
  src/         C++ implementation (logic in src/cpp, public headers in src/include)
  builtin/     registration and thin marshalling of native functions
  functions/   .m functions (the root of this folder is PUBLIC API)
  tests/       .m unit tests for the module
  help/        documentation sources (XML)
  etc/         module configuration (loader.m, ...)
```

# Building

See [BUILDING.md](BUILDING.md) for dependencies and first-time setup, then
the per-platform guides for the developer recipes:

- [BUILDING_Windows.md](BUILDING_Windows.md): full MSBuild command line.
- [BUILDING_Linux.md](BUILDING_Linux.md) and
  [BUILDING_Macos.md](BUILDING_Macos.md): CMake options, debug and
  sanitizer builds.

# Code formatting

- C/C++ uses the WebKit style defined in `.clang-format`. Format everything
  with `just format` (requires `just` and `rust`); never submit unformatted
  C/C++ code.
- Nelson `.m` code is formatted with the built-in `nelson_format` function.
  Files listed in `nelson-format-ignore.json` must not be reformatted.
