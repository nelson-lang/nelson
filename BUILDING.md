# Building Nelson

Depending on what platform or features you require, the build process may
differ slightly. After you've successfully built a binary, running the
test suite to validate that the binary works as intended is a good next step.

If you consistently can reproduce a test failure, search for it in the
[Nelson issue tracker](https://github.com/Nelson-numerical-software/nelson/issues) or
file a new issue.

## How to build Nelson ?

To compile Nelson, you will need a C++17 compiler.
Qt 5.15 or more required.

### [Linux](BUILDING_Linux.md)

### [Mac OS X](BUILDING_Macos.md)

### [Windows](BUILDING_Windows.md)

You can also see [ccpp.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.github/workflows/ccpp.yml) and [appveyor.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/appveyor.yml) files to help you to see dependencies.

[ccpp.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.github/workflows/ccpp.yml) and [appveyor.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/appveyor.yml) files are always more up-to-date than this help.
