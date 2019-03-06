## 0.3.3


Features:
---------

* loadmat: load mat-file into Nelson's workspace

* savemat: save Nelson's workspace to .mat file.

* ismatfile: check if a file is a valid .mat file.


Thanks to MAT file I/O library (MATIO) to provide an easy support for MAT-file.

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
