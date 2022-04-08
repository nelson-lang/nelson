# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## UNRELEASED

### Changed

- Nelson uses [Hack](https://sourcefoundry.org/hack/) font.

- Linux 64 bits and 32 bits uses same main script.

### Added

- 'sscanf' reworked to manage unicode characters and speed optimization.

- French translations imported from Weblate (Thanks to contributors)

### Fixed

- Main Nelson's font was not applied on some OS platforms (ex: MacOS Catalina)

- [#622](http://github.com/Nelson-numerical-software/nelson/issues/622): `isreal(sqrt(i^4))` did not return true.

## 0.6.3 (2022-03-26)

### Changed

- [#596](http://github.com/Nelson-numerical-software/nelson/issues/596): Tests results display use emoji if terminal supports Unicode.

### Added

- Packaging:

  - [#603](http://github.com/Nelson-numerical-software/nelson/issues/603): Nelson as snap package for Linux.
  - [#605](http://github.com/Nelson-numerical-software/nelson/issues/605): [Package request] Chocolatey package manager for Windows.
  - [#582](http://github.com/Nelson-numerical-software/nelson/issues/582): Nelson available as Portable apps.

- add information for Software Center (Linux desktop, icons).

- `nelson` Main script to start Nelson (superceed others scripts).

- `isunicodesupported` function: Detect whether the current terminal supports Unicode.

- `dlsym` function: search nearest symbol name if value entry is not found.

- `terminal_size` function:Query the size of the terminal window.

- [#598](http://github.com/Nelson-numerical-software/nelson/issues/598): `sscanf` function read formatted data from strings.

### Fixed

- [#599](http://github.com/Nelson-numerical-software/nelson/issues/599): `make install` step in CI for linux and MacOs.

- [#601](http://github.com/Nelson-numerical-software/nelson/issues/601): embed all tests on linux and macos install.

## 0.6.2 (2022-02-26)

### Changed

- [#576](http://github.com/Nelson-numerical-software/nelson/issues/576): C++17 Compiler required to build Nelson.

- [#581](http://github.com/Nelson-numerical-software/nelson/issues/581): Github CI platforms list extended (ArchLinux, Fedora, Ubuntu 18.04, MacOs BigSur).

- [#539](http://github.com/Nelson-numerical-software/nelson/issues/539): Visual studio 2022 build on Windows

  - Visual studio 2022 solution upgraded,
  - Github CI and Appveyor use VS 2022 image,
  - boost 1.78 (VS 2022 x86, x64 build),
  - Eigen 3.4 stable branch (Feb 06/22),
  - MSVC 2022 support added to build C/C++ code easily "on fly" on Windows,
  - slicot 5.0 (VS 2022 x86, x64 build),
  - libffi (VS 2022 x86, x64 build),
  - taglib 1.12 (VS 2022 x86, x64 build),
  - hdf5 1.12.1 (VS 2022 x86, x64 build),
  - matio 1.5.21 (VS 2022 x86, x64 build),
  - all others windows dependencies rebuilt with VS 2022.
  - [#505](http://github.com/Nelson-numerical-software/nelson/issues/505): libCurl 7.81 on Windows.
  - [#524](http://github.com/Nelson-numerical-software/nelson/issues/524): oneApi 2022.1 on Windows.

### Added

- Nelson uses `JuliaMono-Regular` font as default.
- [#567](http://github.com/Nelson-numerical-software/nelson/issues/567): `...` in cells if character vector is too long.

### Fixed

- [#587](http://github.com/Nelson-numerical-software/nelson/issues/587): implicit cast to string array for horzcat and vertcat operators.
- [#562](http://github.com/Nelson-numerical-software/nelson/issues/562): `format long` complex do not display expected precision.
- scale factor for integer values did not display as expected.
- [#561](http://github.com/Nelson-numerical-software/nelson/issues/561): `0^0` did not return expected value.
- [#560](http://github.com/Nelson-numerical-software/nelson/issues/560): many warnings fixed (Thanks to new PVS-Studio and cppcheck).
- cmake `WITH_SLICOT`, `WITH_FFTW`, `ENABLE_CLANG_TIDY_FIX` were not documented.
- [#584](http://github.com/Nelson-numerical-software/nelson/issues/584): docker files updated to support C++17 and new libraries.
- [#591](http://github.com/Nelson-numerical-software/nelson/issues/591): Innosetup display glitch with `Nelson's website` button.

## 0.6.1 (2022-01-31)

### Changed

- display of all types reworked to be `pixel perfect`. (a small sentence but a big rework)

### Added

- `format` extended to manage: `compact`, `loose`, `longE`, `longG`, `hex`, `bank`, `rational`.
- [#507](http://github.com/Nelson-numerical-software/nelson/issues/507): `celldisp`: Display cell array contents.
- [#548](http://github.com/Nelson-numerical-software/nelson/issues/548): `hypot` builtin: Square root of sum of squares.
- [#555](http://github.com/Nelson-numerical-software/nelson/issues/555): `rsf2csf` function: Convert real Schur form to complex Schur form.
- `CHANGELOG` 0.6.x family.

## Previous changelog

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
