Features:
---------

  * helper's functions to build C/C++ code easily on Windows, Linux, MacosX
    - dlmake
    - dlgeneratemake
    - dlgeneratecleaner: generates cleaner.nls file for C++ gateway.
    - dlgenerateloader: generates loader.nls file for C++ gateway.
    - dlgenerateunloader: generates unloader.nls file for C++ gateway.
    - dlgenerategateway: generates C++ gateway.
    - cmake function: call CMake tool.
    - findcmake: find CMake path.

  * detect and configure C/C++ compilers on Windows, Linux, MacosX
    - On Windows: 
      - VS 2017 Professional, Entreprise, Community supported.
      - MinGW-W64 for 32 and 64 bit supported.
    - havecompiler function: returns if a compiler is configured.
    - configuremingw function: select and configure Mingw-w64 compilers.
    - configuremsvc function: select and configure Microsoft compilers.
    - loadcompilerconf function: load compiler configuration
    - removecompilerconf function: remove compiler configuration
    - vswhere function: detects easily modern Microsoft compilers.

  * sprintf, fprintf functions: format data into a string or a file.

  * add "<--C/C++ COMPILER REQUIRED-->" tag managed by test engine.

  * add "<--INDEX 64 BIT REQUIRED-->" tag managed by test engine.

  * norm function: matrix and vectors norms.

  * [#128](http://github.com/Nelson-numerical-software/nelson/issues/128): code indented with clang-format (webkit coding style)


Bug Fixes:
---------

  * [#139](http://github.com/Nelson-numerical-software/nelson/issues/139): fix(1e10) returned a wrong value.

  * [#136](http://github.com/Nelson-numerical-software/nelson/issues/136): Corrected predecence of Colon ":" operator.
  
  * [#134](http://github.com/Nelson-numerical-software/nelson/issues/134): Evaluation of Non-Scalar If-Condition Expression was not managed.

  * [#116](http://github.com/Nelson-numerical-software/nelson/issues/116): fix display size of big sparse matrix.


Compilation:
---------

  * libffi 3.2.1 dll on Windows 32 & 64 bit


## 0.2.5 

Features:
---------

  * logm: matrix logarithm.
  * sqrtm: square root of a square matrix.
  * eval function: evaluates string for execution in Nelson.
  * evalin function: evaluates string for execution in Nelson in a specified scope.
  * evalc function: Evaluate Nelson code with console capture.
  * winqueryreg function read the Windows registry (Windows only).
  * factorial function.
  * gamma function.
  * plus, minus, le, ne, ge, gt, lt, eq, colon operators added for all integer types.
  * le, ne, ge, gt, lt, eq for mixed single & double types.
  * add generic overload mechanism for all integers: integer keyword.
  * vertcat and horzcat: mixed concatenations logical with integers, single and double added.
  * [#110](http://github.com/Nelson-numerical-software/nelson/issues/110): add isproperty & ismethod to all handle types.
  * add "isHandleProperty", "isHandleMethod", "getHandleCategory" C++ API Methods.
  * [#108](http://github.com/Nelson-numerical-software/nelson/issues/108): cast function: converts variable to a different data type.
  * MPI Module loaded if MPI dependency is available.

Bug Fixes:
---------

  * [#125](http://github.com/Nelson-numerical-software/nelson/issues/125): cosm function was slow.

  * [#123](http://github.com/Nelson-numerical-software/nelson/issues/123): addpath stopped to work after repeatedly call to the same path.

  * [#121](http://github.com/Nelson-numerical-software/nelson/issues/121): home key did not work in GUI terminal on prompt.

  * [#118](http://github.com/Nelson-numerical-software/nelson/issues/118): add information in  DEVELOPMENT.md about how to buid Boost on old Ubuntu versions.

  * [#109](http://github.com/Nelson-numerical-software/nelson/issues/109): add missing horzcat, vertcat for all handle types.



Compilation:
---------

  * BOOST 1.67 on Windows
  * MSMPI 9.0.1 on Windows
  * Update Visual Studio 15.7.1
   

## 0.2.4 (2018-04-30)


Features:
---------

  * Foreign Function Interface: call C/Fortran functions that are compiled into shared libraries.
    - dlopen: loads an dynamic library. 
    - dlclose: removes/unload dllib object. 
    - dlsym: loads a C/Fortran symbol for an dynamic library.
    - dlcall: C or Fortran Foreign function call. 
    - dllibinfo: returns list of available symbols in an shared library. 
    - dllibisloaded: checks if shared library is loaded.
    - libpointer: creates an C pointer object usuable in Nelson. 
    - getdynlibext : returns the extension of dynamic libraries.


  * [#101](http://github.com/Nelson-numerical-software/nelson/issues/101): allows cell\_vertcat\_generic & cell\_horzcat\_generic.

Bug Fixes:
---------

  [#106](http://github.com/Nelson-numerical-software/nelson/issues/106): update slicot url references.

  [#104](http://github.com/Nelson-numerical-software/nelson/issues/104): mpiexec did not work on some linux.


## 0.2.3 (2018-03-22)


Features:
---------

* 'strlength': length of strings in an cell of strings.
* 'strrep' and 'replace' builtin : replace substring in a string.
* export AUDIODEV=null disables audio module (used for Travis CI)
* 'xmldoctomd' : Converts xml Nelson help files to markdown format.
* 'buildhelpmd' : Build help of Nelson's modules for GitBook.

   see [Nelson's GitBook](https://nelson-numerical-software.gitbooks.io/nelson/content/en/).


## 0.2.2 (2018-02-25)


Features:
---------

  * 'count' : computes the number of occurrences of an pattern.
  * 'startsWith' : checks if string starts with pattern.
  * 'endsWith' : checks if string ends with pattern.
  * 'contains' : checks if string contains a pattern.
  * audiometadata can modify metadata tags.
  * Qt 5.10 used on Windows binaries

Bug Fixes:
---------

  [#98](http://github.com/Nelson-numerical-software/nelson/issues/98): jsonencode was slow with a big file (> 60 Mo).

  [#97](http://github.com/Nelson-numerical-software/nelson/issues/97): fileread was slow with a big file (> 60 Mo).

  [#93](http://github.com/Nelson-numerical-software/nelson/issues/93): playblocking updated to manage range.

  [#92](http://github.com/Nelson-numerical-software/nelson/issues/92): play updated to manage range.

  [#90](http://github.com/Nelson-numerical-software/nelson/issues/90): factorize handle objects methods.


## 0.2.1 (2018-01-30)


Features:
---------

  * pause: pause script execution.
  * keyboard: stop script execution and enter in debug mode.

  * Audio module:
    * beep: do a beep sound.
    * audiodevinfo: Information about audio device.
    * audioplayer: creates an audio player object.
    * play: plays audio object.
    * playblocking: plays audio object and block execution until finish.
    * pause: pauses audio object.
    * stop: stops audio object.
    * resume: resumes audio object.
    * isplaying: returns if audio object is playing.
    * audioread: read an audio file.
    * audioread: write an audio file.
    * audioinfo: get information about audio file.
    * audiometadata: get metadata information about audio file.

  * JSON module: 
    * jsonencode: encodes a Nelson object into a JSON string.
    * jsondecode: decodes a JSON string into a Nelson object.
    * jsonprettyprint: JSON pretty printer.

  * [#89](http://github.com/Nelson-numerical-software/nelson/issues/89): extends fileread behavior.
  
  * 'filewrite' builtin allows to easily write to file a string array or a cell of strings.
  
  * 'newline' function: returns newline character i.e char(10). 
  
  * ndarraychar_disp was missing.

  * NIG slicot uses json files.

Bug Fixes:
---------

  Regenerates help files index if database is empty or wrong (>= Qt 5.9)

  [#87](http://github.com/Nelson-numerical-software/nelson/issues/87): struct did not support sparse matrix.

Compilation:
---------

* Update VS 2017 solution to VS 2017 15.5.4


[Changelog v0.1.x](https://github.com/Nelson-numerical-software/nelson/blob/master/CHANGELOG-0.1.x.md)