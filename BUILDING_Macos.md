### MacOs X

- Prerequirements:
  - install xcode 6.1 or above and command line version
  - install homebrew for mac
    ```bash
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew doctor
    ```
  - install some brew packages
    ```bash
    brew update;
    brew unlink libffi;
    brew install libffi;
    brew unlink libffi;
    brew link --force libffi;
    brew uninstall icu4c;
    brew install icu4c;
    brew unlink icu4c;
    brew link icu4c --force;
    brew info open-mpi;
    brew install openmpi;
    brew install python3;
    brew upgrade python;
    brew install pkg-config;
    brew install pkgconfig;
    brew install gettext;
    brew link --force gettext;
    brew install boost;
    brew install libxml2;
    brew install cmake;
    brew install qt5;
    brew install fftw;
    brew install portaudio;
    brew install libsndfile;
    brew install taglib;
    brew install hdf5;
    brew install libmatio;
    brew install curl;
    brew install libgit2;
    brew install libomp;
    brew install lapack;
    ```
  - Install LAPACK
    By default easier way is to build and install reference LAPACK library (https://github.com/Reference-LAPACK/lapack-release).
    You can also use optimized CPU version with OpenBLAS
    `bash brew install homebrew/science/openblas `

  - Qt 5.6 or more
    You can install from Qt.io or brew
    Current brew version does not package Qt help assistant
    `bash brew install qt5 export PATH=/usr/local/opt/qt5/bin:$PATH `
- Creates Nelson main directory
  ```bash
  mkdir nelson
  cd  nelson
  ```
- Clone mac third party:
  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-macosx.git
  ```
- Get sources:
  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson.git
  ```
- Configure the build:

  ```bash
  cd  nelson
  cmake -G "Unix Makefiles"
  ```

  You can build Nelson under LGPL v2.1 license only with

  ```bash
  cd nelson
  cmake -DLGPL21_ONLY=ON -G "Unix Makefiles" .
  ```

- Start the build:
  ```bash
  make
  ```
- Update localization files if you modify it:
  ```bash
  make updatelocalization
  ```
- build help files:
  ```bash
  make buildhelp
  ```

[Previous (Building)](BUILDING.md)
