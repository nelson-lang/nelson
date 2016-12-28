Great to have you here! Here are a few ways you can help out with [Nelson](https://github.com/Nelson-numerical-software/nelson).

# Where should I start?

## Your first commits

If you’re interested in contributing to Nelson, that’s awesome! We’d love your help.
If you have any questions after reading this page, please feel free to contact [Nelson](https://github.com/Nelson-numerical-software). I will be happy to provide help working through your first bug fix or thinking through the problem you’re trying to resolve.

## How you can help

We track [small bugs and features](https://github.com/Nelson-numerical-software/nelson/issues) so that anyone who wants to help can start with something that's not too overwhelming.
We can absolutely use your help, no matter what level of programming skill you have at the moment.

# Development setup

## How to build Nelson ?

### Linux

* On debian/ubuntu distribution:
    - Prerequirements:
        ```bash
        sudo apt-get install git
        sudo apt-get install pkg-config
        sudo apt-get install libboost1.60-all-dev 
        sudo apt-get install cmake
        sudo apt-get install cmake-gui
        sudo apt-get install libffi-dev
        sudo apt-get install libicu-dev
        sudo apt-get install qtbase5-dev
        sudo apt-get install qttools5-dev-tools
        sudo apt-get install libxml2-dev
        ```
    - Creates Nelson main directory
        ```bash
        mkdir Nelson
        cd  Nelson
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
        cd Nelson
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
        exit
        ```
    - Creates Nelson main directory
        ```bash
        mkdir Nelson
        cd  Nelson
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
        cd Nelson
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


### MacOs

* Prerequirements:
    - install xcode 6.1 and command line version
    - install homebrew for mac
        ```bash
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        brew doctor
        ```
    - install some brew packages
        ```bash
        brew install libffi
        brew install pkg-config libffi 
        brew link --force libffi
        brew install pkgconfig
        brew install icu4c
        brew link icu4c —-force
        brew install gettext
        brew link gettext -—force
        brew install boost
        brew install libxml2
        brew install cmake
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
    mkdir Nelson
    cd  Nelson
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
    cd  Nelson
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
 Visual studio 2015 (C++)
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
    
    start build with VS 2015 

