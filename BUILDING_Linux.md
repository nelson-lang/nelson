### Linux

- You can also see [ccpp.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.github/workflows/ccpp.yml) file to help you to see dependencies. This file is up-to-date about how to build Nelson on each platform.

  Example with latest debian:

  ```bash
  build_debian
  docker run -ti nelson_debian
  ```

- On Fedora distribution: replaces jack-audio-connection-kit-devel package by pipewire-jack-audio-connection-kit-devel

- On debian/ubuntu distribution:

  - Prerequirements:

  ```bash
  sudo apt-get install libopenmpi-dev;
  sudo apt-get install autotools-dev;
  sudo apt-get install libtool;
  sudo apt-get install automake;
  sudo apt-get install openmpi-bin;
  sudo apt-get install gettext;
  sudo apt-get install pkg-config;
  sudo apt-get install cmake;
  sudo apt-get install libffi-dev;
  sudo apt-get install libicu-dev;
  sudo apt-get install libxml2-dev;
  sudo apt-get install liblapack-dev;
  sudo apt-get install liblapacke-dev;
  sudo apt-get install fftw3;
  sudo apt-get install fftw3-dev;
  sudo apt-get install libasound-dev;
  sudo apt-get install portaudio19-dev;
  sudo apt-get install libsndfile1-dev;
  sudo apt-get install libtag1-dev;
  sudo apt-get install alsa-utils;
  sudo apt-get install libhdf5-dev;
  sudo apt-get install hdf5-tools;
  sudo apt-get install zlib1g-dev;
  sudo apt-get install libcurl4-openssl-dev;
  sudo apt-get install libgit2-dev;
  sudo add-apt-repository --yes ppa:ubuntu-sdk-team/ppa;
  sudo apt-get update;
  sudo apt-get install qtbase5-dev qtdeclarative5-dev libqt5webkit5-dev libsqlite3-dev;
  sudo apt-get install qt5-default qttools5-dev-tools;
  sudo apt-get install libqt5qml-graphicaleffects;
  sudo apt-get install libqt5opengl5-dev;
  sudo apt-get install qtbase5-private-dev;
  sudo apt-get install qtdeclarative5-dev;
  sudo apt-get install qtdeclarative5-controls-plugin;
  sudo apt-get install qtdeclarative5-quicklayouts-plugin;
  sudo apt-get install qtdeclarative5-qtquick2-plugin;
  sudo apt-get install qtdeclarative5-dialogs-plugin;
  sudo apt-get install qtdeclarative5-window-plugin;
  ```

  - Get and install Eigen library (minimal 3.3.4)

  ```bash
  git clone https://gitlab.com/libeigen/eigen.git /tmp/eigen && mkdir /tmp/eigen-build && cd /tmp/eigen && git checkout 3.3.4 && cd - && cd /tmp/eigen-build && cmake . /tmp/eigen && make -j4 && sudo make install && cd -;
  ```

  - Get and install matio library (minimal 1.5.15)

  ```bash
  git clone https://github.com/tbeu/matio /tmp/matio && cd /tmp/matio && git checkout v1.5.16 && cd /tmp/matio && ./autogen.sh && ./configure --enable-shared --enable-mat73=yes --enable-extended-sparse=no --with-pic && make && make install
  ```

  - Get and install Boost library (minimal 1.64)

    Boost 1.65 is available on 18.04 LTS [https://packages.ubuntu.com/search?keywords=libboost1.65-all-dev]

  ```bash
      sudo apt-get install libboost1.65-all-dev
  ```

  backports are also available here:

  ```bash
  sudo add-apt-repository --yes ppa:mhier/libboost-latest;
  sudo apt-get update;
  sudo apt-get install libboost1.68-dev;
  ```

  Boost 1.70 requires CMake 3.16 to be correctly detected (see https://apt.kitware.com/).

  ```bash
  sudo apt-add-repository 'deb https://apt.kitware.com/ubuntu/ bionic main'
  sudo apt-get update;
  ```

  or you need to build it:

  ```bash
  export CC=$USE_CC; export CXX=$USE_CXX;
  export BUILD_ROOT=$(pwd);
  cd $HOME;
  curl https://netcologne.dl.sourceforge.net/project/boost/boost/1.65.0/boost_1_65_0.tar.bz2 | tar xj;
  pushd "boost_1_65_0";
  export GCC=$(which $CXX);
  echo -e "using gcc \x3a \x3a $GCC ;" > user-config.jam; cat user-config.jam;
  ./bootstrap.sh --prefix=/usr/local/boost --with-libraries=thread,date_time,filesystem,system,program_options,chrono,regex,locale,iostreams;
  ./b2 -q -d0 --user-config=user-config.jam headers;
  sudo ./b2 -q -d0 --user-config=user-config.jam cxxflags="-std=c++11 -fPIC" threading=multi link=shared install;
  popd;
  ```

  - Creates Nelson main directory

  ```bash
  mkdir nelson
  cd  nelson
  ```

  - Get thirdparty:
  - on Linux 32 bit architecture:

  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-linux32.git
  ```

  - on Linux 64 bit architecture:

  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-linux64.git
  ```

  - Get sources:

  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson.git
  ```

  - Configure the build:

  ```bash
  cd nelson
  cmake -G "Unix Makefiles" .
  ```

  You can build Nelson under LGPL v2.1 license only with

  ```bash
  cd nelson
  cmake -DLGPL21_ONLY=ON -G "Unix Makefiles" .
  ```

  You can build Nelson without FFTW module

  ```bash
  cd nelson
  cmake -DWITH_FFTW=OFF -G "Unix Makefiles" .
  ```

  You can build Nelson without SLICOT module

  ```bash
  cd nelson
  cmake -DWITH_SLICOT=OFF -G "Unix Makefiles" .
  ```

  You can build Nelson with clang-tidy fix

  ```bash
  cd nelson
  cmake -ENABLE_CLANG_TIDY_FIX=ON -G "Unix Makefiles" .
  ```

- Start the build:

```bash
make
```

- Update localization files if you modify it (optional, only for dev):

```bash
make updatelocalization
```

- Build help files:

```bash
make buildhelp
```

[Previous (Building)](BUILDING.md)
