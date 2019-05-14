# 0.3.X

Features:
---------

  * unix, dos, system builtin reworked (asynchronious, better pipes redirection, detached process)

  * FFTW Wrapper allows to load dynamically FFTW library.


Bug Fixes:
---------

  * [#198](http://github.com/Nelson-numerical-software/nelson/issues/198): diary load and save will be disable if nelson is started with '--nouserstartup'.

  * [#196](http://github.com/Nelson-numerical-software/nelson/issues/196): call cmake 3.11 from Nelson fails on linux.


Compilation:
------------

* BOOST 1.64 or more required

* Build on MacOs X 10.13.6 and 10.14.5 (SD notary currently not supporterd.)


# 0.3.4  (2019-04-27)

Features:
---------

* Coverage and Profiling Tools for Nelson's language:

  - profile function: Profile execution time for functions
  - profsave function: Save profile report in HTML format

* blanks builtin: creates an string of blank characters.

* .nh5 files have an header to identify it easily.

* isnh5file, ismatfile extended to return header string.

Bug Fixes:
---------

  * [#193](http://github.com/Nelson-numerical-software/nelson/issues/193): func2str help was wrong.


Compilation:
------------

* MATIO 1.5.15

  Thanks to MAT file I/O library (MATIO) to provide an easy support for MAT-file.

* more 100 warnings fixed (Thanks to PVS-Studio analyzer and also Cppcheck).

* .editorconfig file added.

* Visual studio 15.9.11

* Qt 5.12.2 on Windows



# 0.3.3  (2019-03-21)

Features:
---------

* load, save MAT-files:

  - load overloads loadnh5, loadmat functions.
  - save overloads savenh5, savemat functions.
  - .mat file extension support added: data formatted (Nelson workspace).
  - .mat file association on Windows. load MAT file as data formatted for Nelson.
  - loadmat: load mat-file into Nelson's workspace
  - savemat: save Nelson's workspace to .mat file.
  - rename h5save to savenh5, h5load to loadnh5

* whos: list variables with sizes and types.

  - whosmat: list variables in an valid .mat file with sizes and types.
  - whosnh5: list variables in an valid .nh5 file with sizes and types.

* extends who to manage '-file' option.

  - whomat: list variables in an valid .mat file.
  - whonh5: list variables in an valid .nh5 file.

* ismatfile: check if a file is a valid .mat file.

* isnh5file: check if a file is a valid .nh5 file.


Compilation:
------------

* MATIO 1.5.13

  Thanks to MAT file I/O library (MATIO) to provide an easy support for MAT-file.

* BOOST 1.69 (Windows)

* CMake 3.14 (Windows)


## 0.3.2 (2019-02-24)

Features:
---------

* h5save: save Nelson's workspace to .nh5 file.

* h5load: load data form .nh5 file into Nelson's workspace.

* save, load alias on h5save and h5load.

* .nh5 file extension support added: data formatted (Nelson workspace).

* .nh5 file association on Windows. load data formatted for Nelson.


Compilation:
---------

  * Qt 5.12.1 on Windows

  * fix some 32 bit Warnings.


## 0.3.1 (2019-01-26)


Features:
---------

  * [#173](http://github.com/Nelson-numerical-software/nelson/issues/173): convertStringsToChars and convertCharsToStrings builtin.


Bug Fixes:
---------


  * [#182](http://github.com/Nelson-numerical-software/nelson/issues/182): Nelson did not start without hdf5 dependency.

  * [#179](http://github.com/Nelson-numerical-software/nelson/issues/179): isfolder alias on isdir.

  * [#177](http://github.com/Nelson-numerical-software/nelson/issues/177): some tests failed when it executed from a binary version on windows.

  * [#176](http://github.com/Nelson-numerical-software/nelson/issues/176): nfilename did not return canonical path name in some cases.

  * [#9](http://github.com/Nelson-numerical-software/nelson/issues/9): tests were not embedded in linux & macos binaries (make package).

  * [#4](http://github.com/Nelson-numerical-software/nelson/issues/4): nelson.pot generated from sources.


Compilation:
---------

  * MKL 2019.1 updated for blas, lapack, lapacke, fftw wrappers on Windows.

  * Nelson deployed/installed on appveyor.

  * Nelson available as portable zip file for Windows.

  * Update Visual studio solution to SDK 10.17763
  
  * Update International Components for Unicode 63.1 (Windows and MacOs)


Previous changelog:
---------

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
