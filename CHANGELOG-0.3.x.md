# 0.3.12 (2019-12-27)

- sort builtin: sort double, single, integers, cell and strings.

- diag builtin: Get diagonal elements of matrix or create diagonal matrix.

- Continous Integration tools for external modules (see module skeleton example).

- modules installed with nmm are 'autoload' by default.

- .nmz file extension used as module container.

- nmm('package', module_name, destination_dir) package an external module.

- nmm('install', module.nmz) installs a prebuilt external module.

- '-frozen' option added to addpath builtin.

- rmfield builtin: Remove fields from structure.

- extends sprintf and fprintf to manage backspace characters.

- fix display of vector with NaN or Inf as first element.

## Bug Fixes:

- [#260](http://github.com/nelson-lang/nelson/issues/260): disable files watch for internal modules.

# 0.3.11 (2019-11-26)

- nmm: Nelson Modules Manager (package manager for Nelson)

  - list : get list of installed modules,
  - load : load an installed module for current session,
  - autoload : load modules "marked" as autoload at startup,
  - install : install a distant module,
  - uninstall : uninstall an installed module.

- Module skeleton moved to an dedicated git repository

  - template with builtin and macros: https://github.com/nelson-lang/module_skeleton
  - template with macros only: https://github.com/nelson-lang/module_skeleton_basic

- usermodulesdir builtin: returns directory where user's modules are saved.

- toolboxdir builtin: Root folder for specified toolbox.

- nmm_build_help, nmm_build_loader: helper's functions to build module skeleton.

- semver builtin: semantic versioner.

- executable option added: '--nousermodules' disables load of user's modules.

- add capability to load some user's modules: see nmm('autoload', ...) and nmm('load', ...)

- add // <--NO USER MODULES--> tag for test_run (disable load of user modules for a test)

- fullpath builtin: converts an relative path to full path name.

- getLastReport builtin: returns last formatted error message.

- extends repo to manage plain text authentification.

- repo('export', ...) exports an git repository without .git directory.

- getfield macro replaced by an builtin.

- extends isequal, isequaln, isequalto for structure arrays.

## Bug Fixes:

- [#261](http://github.com/nelson-lang/nelson/issues/261): add a detailed documentation about module.json used in external modules.

- [#259](http://github.com/nelson-lang/nelson/issues/259): extraction decomplexify values.

- [#257](http://github.com/nelson-lang/nelson/issues/257): dllibisloaded optimized.

- [#49](http://github.com/nelson-lang/nelson/issues/49): Some qml demos crashed on Windows 32 bits.

## Compilation:

- Qt 5.13.2 on Windows.

# 0.3.10 (2019-10-29)

- extends 'getmodules' to return module versions using new required 'module.json' (see module's template).

- all core's modules are protected and cannot removed during an nelson's session.

- increase max execution time for tests (2 minutes) and benchs (6 minutes).

- split benchs and tests execution for CI.

- repo builtin: clone, checkout branch or tag, ... from an GIT repository.

## Compilation:

- Visual studio 2019 Community and Pro upgrade (required)
  Dependencies updated:

  - ICU 64.2 on Windows
  - libffi updated VS 2019 build
  - libxml 2.9.9 VS 2019 build
  - libcurl 7.66.0_2
  - CMake 5.15.3 update for Windows
  - MKL 2019 update 5
  - HDF5 1.10.5 VS 2019 build
  - MATIO 1.5.17 VS 2019 build

- appveyor script updated to build with VS 2019

- Innosetup 6 support

## Bug Fixes:

- [#254](http://github.com/nelson-lang/nelson/issues/254): fix Innosetup 6 warnings.

- [#252](http://github.com/nelson-lang/nelson/issues/252): help files of external modules were not loaded.

- [#245](http://github.com/nelson-lang/nelson/issues/245): Update MKL 2019 dependencies.

- [#202](http://github.com/nelson-lang/nelson/issues/202): Migrate to VS 2019.

# 0.3.9 (2019-09-25)

## Features:

- namedargs2cell builtin: Converts a struct containing name-value pairs to a cell.

- extends 'size' builtin to find lengths of multiple array dimensions at a time.

- matches builtin: Determine if pattern matches with strings.

- webwrite function: Write data to RESTful web service.

- improves error message raised by 'run' builtin.

- extends size, length, ndims to manage function_handle type.

- fscanf builtin: Read data from text file.

## Bug Fixes:

- [#249](http://github.com/nelson-lang/nelson/issues/249): refactor code.

- [#236](http://github.com/nelson-lang/nelson/issues/236): mpiexec returned warning on docker as root user.

- [#235](http://github.com/nelson-lang/nelson/issues/235): add example about function_handle with webread.

- [#233](http://github.com/nelson-lang/nelson/issues/233): fix typo in native2unicode help file.

- [#227](http://github.com/nelson-lang/nelson/issues/227): Qt 5.13 support.

- [#220](http://github.com/nelson-lang/nelson/issues/220): setfield function added.

- revert b22ae88a6536cd614f555af7fbd865cc607bea7f due to HDFFV-10579.

- changes file or directory permission had a speed cost.

## Compilation:

- Qt 5.13.0 on Windows.

- GitHub Actions CI (Ubuntu 18.04)

- [#244](http://github.com/nelson-lang/nelson/issues/244): Remove semaphore CI 2.0 build.

# 0.3.8 (2019-08-24)

## Features:

- RESTfull API webservice for Nelson:

  - weboptions function: Set parameters for RESTful API web service.
  - websave builtin: Save content from RESTful API web service to file.
  - webread builtin: Read content from RESTful API web service to nelson's variable.

- UNICODE support extended in Nelson:

  - unicode2native builtin: Converts unicode characters representation to bytes representation.
  - native2unicode builtin: Converts bytes representation representation to unicode string representation.
  - nativecharset builtin: Find all charset matches that appear to be consistent with the input.
  - text editor detects files charset and open files with it.
  - fileread / filewrite builtin extended to use an characters encoding.
  - fopen, fprintf, fgetl, fgets, fread, and fwrite builtin extended to manage characters encoding.

- feof builtin: check for end of file.

- ferror builtin: test for i/o read/write errors.

- tempname function: Returns an unique temporary filename.

- test_run uses nh5 files as result file (previously json)

## Bug Fixes:

- [#226](http://github.com/nelson-lang/nelson/issues/226): tempdir() did not include a final slash.

- [#224](http://github.com/nelson-lang/nelson/issues/224): cd 當第一個按讚的人 crashed Nelson.

## Compilation:

- Visual studio 15.9.14.

- Qt 5.12.4 on Windows.

- CMake 3.9 required on linux and MacOS.

- CircleCI moved to Arch Linux build.

# 0.3.7 (2019-07-23)

## Features:

- dec2base builtin: Convert decimal number to another base.

- dec2bin builtin: Convert decimal number to base 2.

- dec2hex builtin: Convert decimal number to base 16.

- base2dec builtin: Convert number in a base to decimal.

- bin2dec builtin: Convert number in base 2 to decimal.

- hex2dec builtin: Convert number in base 16 to decimal.

- flintmax builtin: Largest consecutive integer in floating-point format.

- realmax builtin: Largest positive floating-point number.

- struct builtin extended to convert object created by 'class' to structure.

## Bug Fixes:

- [#214](http://github.com/nelson-lang/nelson/issues/214): nargin, nargin were slower than 0.3.5.

- [#213](http://github.com/nelson-lang/nelson/issues/213): fix typo in banner help.

## Compilation:

- [#212](http://github.com/nelson-lang/nelson/issues/212): MATIO 1.5.16 used all platforms.

- [#211](http://github.com/nelson-lang/nelson/issues/211): BOOST 1.70 on Windows platforms.

- allocateArrayOf and new_with_exception no more set memory to zero by default. This speed up array constructors.

# 0.3.6 (2019-06-26)

## Features:

- num2bin builtin: Convert number to binary representation.

- bin2num builtin: Convert two's complement binary string to number.

- swapbytes builtin: endian converter.

- zip/unzip builtin: Compress/Uncompress files natively into zip file (with Unicode support).

- license function: Get license information for Nelson.

## Compilation:

- update travis-CI script to support Ubuntu 16.04.

# 0.3.5 (2019-05-26)

## Features:

- Licensing change: Nelson is now released under the terms of the GNU Lesser General Public License (LGPL) v2.1.
  It is still also available under the terms of the GNU General Public License (GPL) v2.0.

  You can build Nelson under LGPL v2.1 license on Unix/MacOS with

  ```bash
  cd nelson
  cmake -DLGPL21_ONLY=ON -G "Unix Makefiles" .
  ```

  On Windows, it is also easy, if you do not select SLICOT library during setup.

- FFTW Wrapper allows to load dynamically FFTW library available on platform.

- SLICOT Wrapper allows to load dynamically SLICOT library available on platform.

- unix, dos, system builtin reworked (asynchronious, better pipes redirection, detached process). This function can be interrupted with CTRL-C key.

- MSVC 2019 support added to build C/C++ code easily "on fly" on Windows.

## Bug Fixes:

- [#198](http://github.com/nelson-lang/nelson/issues/198): history load and save will be disable if nelson is started with '--nouserstartup'.

- [#196](http://github.com/nelson-lang/nelson/issues/196): call cmake 3.11 from Nelson fails on linux.

## Compilation:

- remove hardcoded path between dynamic libraries on linux and macos. It will allow to package nelson easily.

- BOOST 1.64 or more required

- Build on MacOs X 10.13.6 and 10.14.5 (SD notary currently not supporterd.)

- add Dockerfile for Arch, Debian, Fedora images used for CI.

- SLICOT library removed from Nelson's source and moved [here](http://github.com/nelson-lang/slicot_f2c).

# 0.3.4 (2019-04-27)

## Features:

- Coverage and Profiling Tools for Nelson's language:

  - profile function: Profile execution time for functions
  - profsave function: Save profile report in HTML format

- blanks builtin: creates an string of blank characters.

- .nh5 files have an header to identify it easily.

- isnh5file, ismatfile extended to return header string.

## Bug Fixes:

- [#193](http://github.com/nelson-lang/nelson/issues/193): func2str help was wrong.

## Compilation:

- MATIO 1.5.15

  Thanks to MAT file I/O library (MATIO) to provide an easy support for MAT-file.

- more 100 warnings fixed (Thanks to PVS-Studio analyzer and also Cppcheck).

- .editorconfig file added.

- Visual studio 15.9.11

- Qt 5.12.2 on Windows

# 0.3.3 (2019-03-21)

## Features:

- load, save MAT-files:

  - load overloads loadnh5, loadmat functions.
  - save overloads savenh5, savemat functions.
  - .mat file extension support added: data formatted (Nelson workspace).
  - .mat file association on Windows. load MAT file as data formatted for Nelson.
  - loadmat: load mat-file into Nelson's workspace
  - savemat: save Nelson's workspace to .mat file.
  - rename h5save to savenh5, h5load to loadnh5

- whos: list variables with sizes and types.

  - whosmat: list variables in an valid .mat file with sizes and types.
  - whosnh5: list variables in an valid .nh5 file with sizes and types.

- extends who to manage '-file' option.

  - whomat: list variables in an valid .mat file.
  - whonh5: list variables in an valid .nh5 file.

- ismatfile: check if a file is a valid .mat file.

- isnh5file: check if a file is a valid .nh5 file.

## Compilation:

- MATIO 1.5.13

  Thanks to MAT file I/O library (MATIO) to provide an easy support for MAT-file.

- BOOST 1.69 (Windows)

- CMake 3.14 (Windows)

## 0.3.2 (2019-02-24)

## Features:

- h5save: save Nelson's workspace to .nh5 file.

- h5load: load data form .nh5 file into Nelson's workspace.

- save, load alias on h5save and h5load.

- .nh5 file extension support added: data formatted (Nelson workspace).

- .nh5 file association on Windows. load data formatted for Nelson.

## Compilation:

- Qt 5.12.1 on Windows

- fix some 32 bit Warnings.

## 0.3.1 (2019-01-26)

## Features:

- [#173](http://github.com/nelson-lang/nelson/issues/173): convertStringsToChars and convertCharsToStrings builtin.

## Bug Fixes:

- [#182](http://github.com/nelson-lang/nelson/issues/182): Nelson did not start without hdf5 dependency.

- [#179](http://github.com/nelson-lang/nelson/issues/179): isfolder alias on isdir.

- [#177](http://github.com/nelson-lang/nelson/issues/177): some tests failed when it executed from a binary version on windows.

- [#176](http://github.com/nelson-lang/nelson/issues/176): nfilename did not return canonical path name in some cases.

- [#9](http://github.com/nelson-lang/nelson/issues/9): tests were not embedded in linux & macos binaries (make package).

- [#4](http://github.com/nelson-lang/nelson/issues/4): nelson.pot generated from sources.

## Compilation:

- MKL 2019.1 updated for blas, lapack, lapacke, fftw wrappers on Windows.

- Nelson deployed/installed on appveyor.

- Nelson available as portable zip file for Windows.

- Update Visual studio solution to SDK 10.17763

- Update International Components for Unicode 63.1 (Windows and MacOs)

## Previous changelog:

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
