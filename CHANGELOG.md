# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## UNRELEASED

### Changed

- [#576](http://github.com/Nelson-numerical-software/nelson/issues/575): C++17 Compiler required to build Nelson.

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

- [#567](http://github.com/Nelson-numerical-software/nelson/issues/567): `…` in cells if character vector is too long.

### Fixed

- [#562](http://github.com/Nelson-numerical-software/nelson/issues/562): `format long` complex do not display expected precision.
- scale factor for integer values did not display as expected.
- [#561](http://github.com/Nelson-numerical-software/nelson/issues/561): `0^0` did not return expected value.
- [#560](http://github.com/Nelson-numerical-software/nelson/issues/560): many warnings fixed (Thanks to new PVS-Studio and cppcheck).

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
