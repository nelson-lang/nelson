
         __     _  __
      /\ \ \___| |/ _\ ___  _ __
     /  \/ / _ | |\ \ / _ \| '_ \
    / /\  |  __| |_\ | (_) | | | |
    \_\ \/ \___|_|\__/\___/|_| |_|

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/8865bc705b2d459c839b169e580d0526)](https://www.codacy.com/app/Nelson-numerical-software/nelson?utm_source=github.com&utm_medium=referral&utm_content=Nelson-numerical-software/nelson&utm_campaign=badger)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/602/badge)](https://bestpractices.coreinfrastructure.org/projects/602) 
[![GitHub license](https://img.shields.io/badge/license-GPL2-blue.svg)](https://github.com/Nelson-numerical-software/nelson/blob/master/COPYING.md)
[![CLA assistant](https://cla-assistant.io/readme/badge/Nelson-numerical-software/nelson)](https://cla-assistant.io/Nelson-numerical-software/nelson)
[![Open Hub](https://img.shields.io/badge/Open-Hub-blue.svg)](https://www.openhub.net/p/nelson-interpreter)
[![Weblate](https://img.shields.io/badge/Weblate--green.svg)](https://hosted.weblate.org/projects/nelson/)
[![Build Status](https://travis-ci.org/Nelson-numerical-software/nelson.svg?branch=master)](https://travis-ci.org/Nelson-numerical-software/nelson)
[![Build status](https://ci.appveyor.com/api/projects/status/github/Nelson-numerical-software/nelson?svg=true)](https://ci.appveyor.com/project/Nelson-numerical-software/nelson)
[![Total alerts](https://img.shields.io/lgtm/alerts/g/Nelson-numerical-software/nelson.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/Nelson-numerical-software/nelson/alerts/)

[![CircleCI](https://circleci.com/gh/Nelson-numerical-software/nelson/tree/master.svg?style=svg)](https://circleci.com/gh/Nelson-numerical-software/nelson/tree/master)
[![Build Status](https://semaphoreci.com/api/v1/nelson-numerical-software/nelson/branches/master/badge.svg)](https://semaphoreci.com/nelson-numerical-software/nelson)


[![Join the chat at https://gitter.im/nelson-numerical-software/Lobby](https://badges.gitter.im/nelson-numerical-software/Lobby.svg)](https://gitter.im/nelson-numerical-software/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Nelson is an array programming language providing a powerful open computing environment for 
engineering and scientific applications using modern C/C++ libraries (Boost, Eigen, …)
and others state of art numerical libraries.

It has sophisticated data structures (including cell, struct, linear systems, …),
an interpreter and a high level programming language.

Nelson has been developped to be an open/modular system where an user can define 
these own data types and operations on these data types by using overload.

[Web site](https://nelson-numerical-software.github.io/nelson-website/)


## Features:

- Types managed by Nelson:
  * double and double complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  * single and single complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix. 
  * logical: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  * character array (UNICODE supported).
  * string array (UNICODE supported).
  * integers 8, 16, 32, 64 signed and unsigned: scalar, vector, matrix 2D, N dimensions array.
  * handle objects.

- Fast Fourrier Transformation functions based on FFTW. 

- SLICOT (Subroutine Library in Systems and Control Theory) interfaces.

- Message Passing Interface (MPI): functions for parallel computing.

- JSON decode/encode data support.

- Foreign Function Interface C/Fortran.

- Interfacing C/C++ or Fortran with Nelson (build and load external code on the fly).

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
 
## Resources

- **Homepage:** <https://nelson-numerical-software.github.io/nelson-website/>
- **Source code:** <https://github.com/Nelson-numerical-software/nelson>
- **Binaries:** <https://github.com/Nelson-numerical-software/nelson/releases>
- **Docker:** <https://hub.docker.com/r/nelsonsoftware/nelson/>
- **Documentation:** <https://nelson-numerical-software.github.io/nelson-website/help/en_US/>
- **GitBook:**
<https://nelson-numerical-software.gitbooks.io/nelson/content/en/>
- **Gitter:** <https://gitter.im/nelson-numerical-software/Lobby>
- **YouTube:** <https://www.youtube.com/channel/UCdZMnH0HC9XflNGAFFiRX9g>
- **Twitter:** <https://twitter.com/Nelson_software>


## Tested and supported platforms:
- Windows 8, 10 32 bit & 64 bit (intel x86)
- Linux Debian 32 bit & 64 bit (intel x86)
- Mac Os X Sierra (intel x86)
- Linux Ubuntu 14.04 and more (Travis CI, CircleCI, Semaphore)
- Arch Linux (ARM v7 i.MX 6)
- Raspberry Pi 3 Model B ( Raspbian)
- Rock64 (Ubuntu aarch64)


Allan CORNET (nelson.numerical.computation@gmail.com)

