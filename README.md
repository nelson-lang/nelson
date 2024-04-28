![Nelson logo](https://github.com/nelson-lang/nelson/blob/master/resources/banner_nelson_small.png?raw=true)

![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=nelson-lang.nelson)

[![Codacy Badge](https://app.codacy.com/project/badge/Grade/d5f82474da134d979b472fa5fbe7b5b9)](https://www.codacy.com/gh/nelson-lang/nelson/dashboard?utm_source=github.com&utm_medium=referral&utm_content=nelson-lang/nelson&utm_campaign=Badge_Grade)
[![CodeQL analysis (C/C++)](https://github.com/nelson-lang/nelson/actions/workflows/codeql-analysis-cpp.yml/badge.svg)](https://github.com/nelson-lang/nelson/actions/workflows/codeql-analysis-cpp.yml)[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/602/badge)](https://bestpractices.coreinfrastructure.org/projects/602)

[![Build Status](https://github.com/nelson-lang/nelson/workflows/C%2FC%2B%2B%20CI/badge.svg)](https://github.com/nelson-lang/nelson/workflows/C%2FC%2B%2B%20CI/badge.svg)

[![GitHub license](https://img.shields.io/badge/license-LGPL3.0-blue.svg)](https://github.com/nelson-lang/nelson/blob/master/lgpl-3.0.md)
[![CLA assistant](https://cla-assistant.io/readme/badge/nelson-lang/nelson)](https://cla-assistant.io/nelson-lang/nelson)
[![Open Hub](https://img.shields.io/badge/Open-Hub-blue.svg)](https://www.openhub.net/p/nelson-interpreter)
[![Weblate](https://img.shields.io/badge/Weblate--green.svg)](https://hosted.weblate.org/projects/nelson/)

[![Join the chat at https://gitter.im/nelson-lang/Lobby](https://badges.gitter.im/nelson-lang/Lobby.svg)](https://gitter.im/nelson-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Nelson is an array programming language providing a powerful open computing environment for
engineering and scientific applications using modern C/C++ libraries (Boost, Eigen, …)
and others state of art numerical libraries.

It has sophisticated data structures (including cell, struct, linear systems, …),
an interpreter and a high level programming language.

Nelson has been developed to be an open/modular system where an user can define
these own data types and operations on these data types by using overload.

![Nelson environment](https://github.com/nelson-lang/nelson-website/blob/master/images/Nelson-windows.png?raw=true)

## Website dedicated to users

For more information go to [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/).

## Starring the repository

If you found this project useful, please consider [starring it on GitHub !!!](https://github.com/nelson-lang/nelson/stargazers) This allows me to see how many people are using my code, and motivates me to keep working to improve it.

## Download

[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-black.svg)](https://snapcraft.io/nelson)

[Latest Windows Installer](https://github.com/nelson-lang/nelson/releases)

[Get it from the Windows Package Manager](https://winstall.app/apps/NelsonNumericalSoftware.Nelson)

[Get it from the Chocolatey Package Manager](https://community.chocolatey.org/packages/nelson)

## Features

- Types managed by Nelson:

  - double and double complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - single and single complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - logical: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - character array (UNICODE supported).
  - string array (UNICODE supported).
  - integers 8, 16, 32, 64 signed and unsigned: scalar, vector, matrix 2D, N dimensions array.
  - handle objects.
  - anonymous functions,
  - all types can be overloaded.

- `OpenMP` and `SIMD` extensions used.

- 2D and 3D plotting with high-level plot commands.

- Desktop environment with history, file and workspace browser.

- Parallel Computing Module.

- Fast Fourrier Transformation functions based on FFTW and MKL wrapper.

- SLICOT (Subroutine Library in Systems and Control Theory) interfaces (optional).

- Control System module.

- Message Passing Interface (MPI): functions for parallel computing.

- JSON decode/encode data support.

- HDF5 high-level functions I/O,

- HDF5 used as default data file format (.nh5) load/save workspace,

- MAT-file compatible load/save workspace,

- Foreign Function Interface C/Fortran.

- Interfacing C/C++ or Fortran with Nelson (build and load external code on the fly).

- MEX C API compatibility.

- Nelson Engine API for C (compatible with MEX Engine). Call Nelson from your C code as engine.

- Call Python from Nelson.

- RESTful API web service.

- Inter-process communication between Nelson's process.

- The QML engine enables nelson programs to display and manipulate graphical content using Qt's QML framework.

- Component Object Model (COM) client interface: binary-interface standard for software components on Windows.

- Write/Read xlsx files on Windows using COM.

- Embedded Nelson code editor.

- Help engine:

  Generate help files using Nelson dedicated functions.
  View your generated help files as html, markdown, pdf, gitbook or directly in Nelson help viewer.

- Tests engine:

  Validate your algorithm using Nelson dedicated functions.
  Export the test results under the xUnit reports format.

- Profiling and Code coverage tools for Nelson's language:

  Nelson has a built-in profiler that is very useful to profile your code and find out what script or function is taking the most time.

- [Nelson cloud](https://www.npmjs.com/package/nelson-cloud):
  Instant access to Nelson anywhere from an web browser.

- Module skeleton to extend Nelson available here:

  - [template macros and builtin](https://github.com/nelson-lang/module_skeleton),
  - [basic template macros only](https://github.com/nelson-lang/module_skeleton_basic).

- Nelson Modules Manager (nmm) : package manager for Nelson

## Licenses

Nelson is delivered under dual-license:

- [![License (GNU Lesser General Public License (LGPL) v3.0)](<https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20(LGPL)%20v3.0-blue.svg?style=flat-square>)](https://opensource.org/licenses/LGPL-3.0)

  or

- [![License (GNU General Public License (GPL) v3.0)](<https://img.shields.io/badge/license-GNU%20General%20Public%20License%20(GPL)%20v3.0-blue.svg?style=flat-square>)](https://opensource.org/licenses/GPL-3.0)

## Resources

- **Homepage:** <https://nelson-lang.github.io/nelson-website/>
- **Source code:** <https://github.com/nelson-lang/nelson>
- **Binaries:** <https://github.com/nelson-lang/nelson/releases>
- **Docker:** <https://hub.docker.com/r/nelsonsoftware/nelson/>
- **Documentation:** <https://nelson-lang.github.io/nelson-website/help/en_US/>
- **GitBook:**
  <https://nelson-9.gitbook.io/nelson/>
- **Gitter:** <https://gitter.im/nelson-lang/Lobby>
- **YouTube:** <https://www.youtube.com/channel/UCdZMnH0HC9XflNGAFFiRX9g>
- **Twitter:** <https://twitter.com/Nelson_software>

## Tested and supported platforms

Systems with ✓ downloads are available on [downloads](https://github.com/nelson-lang/nelson/releases) page.

Systems with ✓ for CI are tested with continuous integration for every commit.

The systems listed here without CI or binaries are known to build and operate, but the continued support of these platforms is user-dependent.

| Operating System |  Architecture   | CI  | Binaries |
| :--------------: | :-------------: | :-: | :------: |
| Windows 7 to 11  | x86-64 (64-bit) |  ✓  |    ✓     |
|                  |  i686 (32-bit)  |  ✓  |    ✓     |
|   macOS Sonoma   |   arm64 (M1)    |  ✓  |          |
|  macOS Ventura   | x86-64 (64-bit) |  ✓  |          |
|  macOS Monterey  | x86-64 (64-bit) |  ✓  |          |
|   Ubuntu 24.04   | x86-64 (64-bit) |     | ✓ (snap) |
|   Ubuntu 22.04   | x86-64 (64-bit) |  ✓  | ✓ (snap) |
|   Ubuntu 20.04   | x86-64 (64-bit) |  ✓  | ✓ (snap) |
|   Ubuntu 18.04   | x86-64 (64-bit) |     | ✓ (snap) |
|    ArchLinux     | x86-64 (64-bit) |  ✓  | ✓ (snap) |
|    Fedora 36     | x86-64 (64-bit) |  ✓  | ✓ (snap) |
|     Raspbian     | ARM v8 (64-bit) |     |          |

Allan CORNET (nelson.numerical.computation@gmail.com)
