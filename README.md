![Nelson logo](https://github.com/Nelson-numerical-software/nelson/blob/master/resources/banner_nelson_small.png?raw=true)

[![Codacy Badge](https://app.codacy.com/project/badge/Grade/d5f82474da134d979b472fa5fbe7b5b9)](https://www.codacy.com/gh/Nelson-numerical-software/nelson/dashboard?utm_source=github.com&utm_medium=referral&utm_content=Nelson-numerical-software/nelson&utm_campaign=Badge_Grade)
[![Total alerts](https://img.shields.io/lgtm/grade/cpp/g/Nelson-numerical-software/nelson.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/Nelson-numerical-software/nelson/alerts/)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/602/badge)](https://bestpractices.coreinfrastructure.org/projects/602)

[![Build Status](https://github.com/Nelson-numerical-software/nelson/workflows/C%2FC%2B%2B%20CI/badge.svg)](https://github.com/Nelson-numerical-software/nelson/workflows/C%2FC%2B%2B%20CI/badge.svg)
[![Build Status](https://travis-ci.org/Nelson-numerical-software/nelson.svg?branch=master)](https://travis-ci.org/Nelson-numerical-software/nelson)
[![Build status](https://ci.appveyor.com/api/projects/status/github/Nelson-numerical-software/nelson?svg=true)](https://ci.appveyor.com/project/Nelson-numerical-software/nelson)
[![CircleCI](https://circleci.com/gh/Nelson-numerical-software/nelson/tree/master.svg?style=svg)](https://circleci.com/gh/Nelson-numerical-software/nelson/tree/master)
[![Build Status](https://semaphoreci.com/api/v1/nelson-numerical-software/nelson/branches/master/badge.svg)](https://semaphoreci.com/nelson-numerical-software/nelson)

[![GitHub license](https://img.shields.io/badge/license-LGPL2.1-blue.svg)](https://github.com/Nelson-numerical-software/nelson/blob/master/COPYING.LGPLv2.1)
[![CLA assistant](https://cla-assistant.io/readme/badge/Nelson-numerical-software/nelson)](https://cla-assistant.io/Nelson-numerical-software/nelson)
[![Open Hub](https://img.shields.io/badge/Open-Hub-blue.svg)](https://www.openhub.net/p/nelson-interpreter)
[![Weblate](https://img.shields.io/badge/Weblate--green.svg)](https://hosted.weblate.org/projects/nelson/)

[![Join the chat at https://gitter.im/nelson-numerical-software/Lobby](https://badges.gitter.im/nelson-numerical-software/Lobby.svg)](https://gitter.im/nelson-numerical-software/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Nelson is an array programming language providing a powerful open computing environment for
engineering and scientific applications using modern C/C++ libraries (Boost, Eigen, …)
and others state of art numerical libraries.

It has sophisticated data structures (including cell, struct, linear systems, …),
an interpreter and a high level programming language.

Nelson has been developed to be an open/modular system where an user can define
these own data types and operations on these data types by using overload.

![Nelson environment](https://github.com/Nelson-numerical-software/nelson-website/blob/master/images/Nelson-windows.png?raw=true)

[Web site](https://nelson-numerical-software.github.io/nelson-website/)

## Features

- Types managed by Nelson:

  - double and double complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - single and single complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - logical: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - character array (UNICODE supported).
  - string array (UNICODE supported).
  - integers 8, 16, 32, 64 signed and unsigned: scalar, vector, matrix 2D, N dimensions array.
  - handle objects.

- `OpenMP` and `SIMD` extensions used.

- Fast Fourrier Transformation functions based on FFTW and MKL wrapper.

- SLICOT (Subroutine Library in Systems and Control Theory) interfaces (optional).

- Message Passing Interface (MPI): functions for parallel computing.

- JSON decode/encode data support.

- HDF5 high-level functions I/O,

- HDF5 used as default data file format (.nh5) load/save workspace,

- MAT-file compatible load/save workspace,

- Foreign Function Interface C/Fortran.

- Interfacing C/C++ or Fortran with Nelson (build and load external code on the fly).

- MEX C API compatibility.

- Nelson Engine API for C (compatible with MEX Engine). Call Nelson from your C code as engine.

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

  - [template macros and builtin](https://github.com/Nelson-numerical-software/module_skeleton),
  - [basic template macros only](https://github.com/Nelson-numerical-software/module_skeleton_basic).

- Nelson Modules Manager (nmm) : package manager for Nelson

## Licenses

Nelson is delivered under dual-license:

- [![License (GNU Lesser General Public License (LGPL) v2.1)](<https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20(LGPL)%20v2.1-blue.svg?style=flat-square>)](https://opensource.org/licenses/LGPL-2.1)

  or

- [![License (GNU General Public License (GPL) v2.0)](<https://img.shields.io/badge/license-GNU%20General%20Public%20License%20(GPL)%20v2-blue.svg?style=flat-square>)](https://opensource.org/licenses/GPL-2.0)

## Resources

- **Homepage:** <https://nelson-numerical-software.github.io/nelson-website/>
- **Source code:** <https://github.com/Nelson-numerical-software/nelson>
- **Binaries:** <https://github.com/Nelson-numerical-software/nelson/releases>
- **Docker:** <https://hub.docker.com/r/nelsonsoftware/nelson/>
- **Documentation:** <https://nelson-numerical-software.github.io/nelson-website/help/en_US/>
- **GitBook:**
  <https://nelson-9.gitbook.io/nelson/>
- **Gitter:** <https://gitter.im/nelson-numerical-software/Lobby>
- **YouTube:** <https://www.youtube.com/channel/UCdZMnH0HC9XflNGAFFiRX9g>
- **Twitter:** <https://twitter.com/Nelson_software>

## Tested and supported platforms

Systems with ✓ downloads are available on [downloads](https://github.com/Nelson-numerical-software/nelson/releases) page.

Systems with ✓ for CI are tested with continuous integration for every commit.

The systems listed here without CI or binaries are known to build and operate, but the continued support of these platforms is user-dependent.

| Operating System |  Architecture   | CI  | Binaries |
| :--------------: | :-------------: | :-: | :------: |
|    Windows 7+    | x86-64 (64-bit) |  ✓  |    ✓     |
|                  |  i686 (32-bit)  |  ✓  |    ✓     |
|   MacOS 10.10+   | x86-64 (64-bit) |  ✓  |          |
|   Ubuntu 20.04   | x86-64 (64-bit) |     |          |
|   Ubuntu 18.04   | x86-64 (64-bit) |  ✓  |          |
|   Ubuntu 16.04   | x86-64 (64-bit) |  ✓  |          |
|    Debian 10     | x86-64 (64-bit) |  ✓  |          |
|    Fedora 30     | x86-64 (64-bit) |  ✓  |          |
|    ArchLinux     | x86-64 (64-bit) |  ✓  |          |
|                  | ARM v7 (32-bit) |     |          |
|                  | ARM v8 (64-bit) |     |          |
| Ubuntu (aarch64) | ARM v8 (64-bit) |     |          |
|     Raspbian     | ARM v7 (32-bit) |     |          |

Allan CORNET (nelson.numerical.computation@gmail.com)
