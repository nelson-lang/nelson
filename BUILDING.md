# Building Nelson

Depending on what platform or features you require, the build process may
differ slightly. After you've successfully built a binary, running the
test suite to validate that the binary works as intended is a good next step.

If you consistently can reproduce a test failure, search for it in the
[Nelson issue tracker](https://github.com/Nelson-numerical-software/nelson/issues) or
file a new issue.

## How to build Nelson ?

You can also see [.travis.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.travis.yml) file to help you to see dependencies.

 [.travis.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.travis.yml) file is always more up-to-date than this help.

### Linux

* On debian/ubuntu distribution:
    - Prerequirements:
        ```bash
      sudo apt-get install libopenmpi-dev
      sudo apt-get install openmpi-bin
      sudo apt-get install gettext
      sudo apt-get install pkg-config
      sudo apt-get install cmake
      sudo apt-get install libffi-dev
      sudo apt-get install libicu-dev
      sudo apt-get install libxml2-dev
      sudo apt-get install liblapack-dev
      sudo apt-get install liblapacke-dev
      sudo apt-get install fftw3
      sudo apt-get install fftw3-dev
      sudo apt-get install libasound-dev
      sudo apt-get install portaudio19-dev
      sudo apt-get install libsndfile1-dev
      sudo apt-get install libtag1-dev
      sudo apt-get install alsa-utils
      sudo add-apt-repository --yes ppa:ubuntu-sdk-team/ppa
      sudo apt-get update
      sudo apt-get install qtbase5-dev qtdeclarative5-dev libqt5webkit5-dev libsqlite3-dev
      sudo apt-get install qt5-default qttools5-dev-tools
      sudo apt-get install libqt5qml-graphicaleffects
      sudo apt-get install libqt5opengl5-dev
      sudo apt-get install qtbase5-private-dev
      sudo apt-get install qtdeclarative5-dev
      sudo apt-get install qtdeclarative5-controls-plugin
      sudo apt-get install qtdeclarative5-quicklayouts-plugin
      sudo apt-get install qtdeclarative5-qtquick2-plugin
      sudo apt-get install qtdeclarative5-dialogs-plugin
      sudo apt-get install qtdeclarative5-window-plugin      
        ```

    - Get and install Eigen library (minimal 3.3.4)
        ```bash
        git clone https://github.com/eigenteam/eigen-git-mirror /tmp/eigen && mkdir /tmp/eigen-build && cd /tmp/eigen && git checkout 3.3.4 && cd - && cd /tmp/eigen-build && cmake . /tmp/eigen && make -j4 && sudo make install && cd -;
        ```

    - Get and install Boost library (minimal 1.62)
    
        Boost 1.62 is available on Ubuntu 17.01 & 18.04 LTS [https://packages.ubuntu.com/search?keywords=libboost1.62-all-dev]
        ```bash
        sudo apt-get install libboost1.62-all-dev
        ```
        or you need to build it:
        ```bash
        export CC=$USE_CC; export CXX=$USE_CXX;
        export BUILD_ROOT=$(pwd);
        cd $HOME;
        curl https://netcologne.dl.sourceforge.net/project/boost/boost/1.62.0/boost_1_62_0.tar.bz2 | tar xj;
        pushd "boost_1_62_0";
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
        cmake -G "Unix Makefiles"
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

* on ArchLinux (ARM v7)
    - Prerequirements:
        ```bash
        su
        pacman -S pkg-config
        pacman -S boost-libs boost
        pacman -S cmake
        pacman -S libffi
        pacman -S icu
        pacman -S qt5-base
        pacman -S qt5-tools
        pacman -S qt5-webkit
        pacman -S libxml2
        pacman -S gcc
        pacman -S make
        pacman -S blas
        pacman -S lapack
        pacman -S lapacke
        pacman -S fftw
        pacman -S openmpi
        exit
        ```
    - Creates Nelson main directory
        ```bash
        mkdir nelson
        cd  nelson
        ```
    - Get thirdparty:
        ```bash
        git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-linux32.git
        ```
    - Get sources:
        ```bash
        git clone https://github.com/Nelson-numerical-software/nelson.git
        ```
    - Configure the build:
        ```bash
        cd nelson
        cmake -G "Unix Makefiles"
        ```
    - Start the build:
        ```bash
        make
        ```
    - Update localization files if you modify it:
        ```bash
        make updatelocalization
        ```
    - Build help files:
        ```bash
        make buildhelp
        ```


### MacOs X

* Prerequirements:
    - install xcode 6.1 or above and command line version
    - install homebrew for mac
        ```bash
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        brew doctor
        ```
    - install some brew packages
        ```bash
      brew update;
      brew info open-mpi;
      brew install openmpi;
      brew install python3;
      brew upgrade python;
      brew install libffi;
      brew install icu4c;
      brew link --force icu4c;
      brew install pkg-config;
      brew link --force libffi;
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
         ```
    - Install LAPACKE
    By default easier way is to build and install reference LAPACK library (https://github.com/Reference-LAPACK/lapack-release).
    LAPACKE prebuild is available for macos Sierra in nelson-thirdparty-macosx git repository.
    You can also use optimized CPU version with OpenBLAS
        ```bash
        brew install homebrew/science/openblas
        ```
    
    - Qt 5.6 or more
    You can install from Qt.io or brew
    Current brew version does not package Qt help assistant
        ```bash
        brew install qt5
        export PATH=/usr/local/opt/qt5/bin:$PATH
        ```
* Creates Nelson main directory
    ```bash
    mkdir nelson
    cd  nelson
    ```
* Clone mac third party:
    ```bash
    git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-macosx.git
    ```
* Get sources:
    ```bash
    git clone https://github.com/Nelson-numerical-software/nelson.git
    ```
* Configure the build:
    ```bash
    cd  nelson
    cmake -G "Unix Makefiles" 
    ```
* Start the build:
    ```bash
    make
    ```
* Update localization files if you modify it:
    ```bash
    make updatelocalization
    ```
* build help files:
    ```bash
    make buildhelp
    ```

### Windows

* Prerequirements:
 Git for Windows
 Visual studio 2017 (C++)
 Qt 5.x for Windows

* Creates Nelson main directory
    ```bash
    mkdir Nelson
    cd  Nelson
    ```
* Get thirdparty:
    - on Windows 32 bit architecture:
        ```bash
        git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-win32.git
        ```
    - on Windows 64 bit architecture:
        ```bash
        git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-x64.git
        ```
* Get sources:
    ```bash
    git clone https://github.com/Nelson-numerical-software/nelson.git
    ```
    
* Start the build:
    Goto Nelson directory
    
    set QTDIR, QTDIR32, QTDIR64 environment variables
    
    launch win32-environment.bat (win32 build) or win64-environment.bat (win64 build)
    
    start build with VS 2017 


