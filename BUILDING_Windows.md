### Windows

- Prerequirements:
  Git for Windows
  Visual studio 2019 (C++)
  Qt 5.x for Windows

- Creates Nelson main directory
  ```bash
  mkdir Nelson
  cd  Nelson
  ```
- Get thirdparty:
  - on Windows 32 bit architecture:
    ```bash
    git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-win32.git
    ```
  - on Windows 64 bit architecture:
    ```bash
    git clone https://github.com/Nelson-numerical-software/nelson-thirdparty-x64.git
    ```
- Get sources:
  ```bash
  git clone https://github.com/Nelson-numerical-software/nelson.git
  ```
- Start the build:
  Goto Nelson directory
  set QTDIR, QTDIR32, QTDIR64 environment variables
  launch win32-environment.bat (win32 build) or win64-environment.bat (win64 build)
  start build with VS 2019

[Previous (Building)](BUILDING.md)
