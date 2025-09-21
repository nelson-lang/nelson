# 0.4.12 (2020-12-30)

- `eig` builtin: Eigenvalues and eigenvectors.

- `det` builtin: Matrix determinant.

- `gcd` builtin: Greatest common divisor.

- `find` builtin: Find indices and values of nonzero elements.

- `ishermitian` builtin: Checks if an matrix is hermitian or skew-hermitian.

- `strcat` builtin: concatenate strings horizontally.

- `append` builtin: combine strings horizontally.

- `corrcoef` function: correlation coefficients.

- `cov` function: covariance.

- `var` builtin: variance.

- `magic` function: magic square.

- `mpower` builtin: matrix support added.

- `fft` is faster: plan was not correctly cached.

- `|`, `&`, `./` and `.^` operators are faster.

- `inv` is faster.

- extends `fullfile` compatibility with string type.

- extends `assert_isequal`, `isequal` to manage missing type.

- extends `issymmetric` to manage boolean type.

## Bug Fixes:

- [#364](https://github.com/nelson-lang/nelson/issues/364): `isinf`, `isnan`, `conj`, `double`, `single`, `real`, `imag` are faster.

- [#361](https://github.com/nelson-lang/nelson/issues/361): `abs` is faster.

- [#360](https://github.com/nelson-lang/nelson/issues/360): `ctranpose` and `transpose` are faster.

- [#353](https://github.com/nelson-lang/nelson/issues/353): `N = i; N(1)` returned wrong value.

- [#351](https://github.com/nelson-lang/nelson/issues/351): binary operators and empty matrix (behavior described in book of Carl de Boor in An Empty Exercise)

## Compilation:

- Qt 5.15.2 on Windows (AppVeyor CI).

# 0.4.11 (2020-11-24)

- Nelson Engine API for C (compatible with MEX Engine 100%).

  - engSetVisible,
  - engGetVisible,
  - engEvalString,
  - engOutputBuffer.

- `sha256` builtin: get sha256 checksum of a file or a string.

- `ipc` extended with `minimize` argument.

- `fullfile` builtin: build full file name from parts.

## Bug Fixes:

- [#342](https://github.com/nelson-lang/nelson/issues/342): disable slicot on Macos CI.

- [#341](https://github.com/nelson-lang/nelson/issues/341): extend `ipc(pid, 'post', cmd, scope)` to manage scope destination.

- [#314](https://github.com/nelson-lang/nelson/issues/314): Nelson crashs randomly at exit with Qt 5.15.0

## Compilation:

- Qt 5.15.1 on Windows (AppVeyor CI).

# 0.4.10 (2020-10-29)

- [IN PROGRESS] Nelson Engine API for C (compatible with MEX Engine).

  - engOpen,
  - engOpenSingleUse,
  - engClose,
  - engEvalString,
  - engPutVariable,
  - engGetVariable.

- extends `mex` function to generate also executable.

- extends `dlgeneratemake` function to generate also executable.

- `--minimize` command line argument added. minimize main GUI Window at startup.

## Bug Fixes:

- [#340](https://github.com/nelson-lang/nelson/issues/340): `evalin` did not restore correctly variables after call.

- [#339](https://github.com/nelson-lang/nelson/issues/339): `cd`, `dir`, `ls` had some compatibility troubles.

- [#332](https://github.com/nelson-lang/nelson/issues/332): removes connect(2) call to /dev/shm/jack-0/default/jack_0 failed (err=No such file or directory).

- [#331](https://github.com/nelson-lang/nelson/issues/331): move ipc features to detected module

## Compilation:

- SEMAPHORE CI platform updated to Ubuntu 14.04 - GCC GNU 4.8.4 (supported until it is no more possible)

- cmake binaries_directory supported (LGTM support).

- fix some warnings detected with LGTM.

- Eigen 3.3.8 stable on all platforms (mirror url also updated).

# 0.4.9 (2020-09-27)

- ipc builtin: Inter-process communication between Nelson's process

- getpid('running') renamed getpid('available').

- --noipc command line argument added. disable IPC features.

- dark theme detected and used on Macos X.

- test_run reworked (faster to start).

- test_run extended with '-stoponfail' option.

- jsonencode faster for string encoding.

- rework timeout thread.

## Bug Fixes:

- [#330](https://github.com/nelson-lang/nelson/issues/330): removes ALSA errors and warnings on linux.

## Compilation:

- [#322](https://github.com/nelson-lang/nelson/issues/322): fix build with gcc 4.8 (ubuntu 14.04).

  Nelson 0.4.9 will be last to support gcc 4.8

- libsndfile 1.0.30 on Windows.

# 0.4.8 (2020-08-26)

- multiplatforms files association based on Inter-process communication.
  open, load, execute files in latest created Nelson's process.

- event loop and command queue updated.

- fix play, playblocking, resume builtin.

- getpid() returns current process identificator.

- getpid('running') returns all nelson processes identificators currently running for current user.

- hostname() returns current host name of your computer.

- username() returns current user name used on your computer.

- isvector checks if input is an vector.

## Compilation:

- libffi 3.3 was not detected on Macos X.

- libicu4c 67.1 was not detected on Macos X.

- Qt 5.15 official package was not detected on Macos X.

- libsndfile 1.0.29 on Windows.

- libcurl 7.72.0 on Windows.

- libbost 1.74.0 on Windows.

- CMake 3.18.1 used on Windows.

- GitHub CI MacOS x Catalina.

# 0.4.7 (2020-07-31)

- [#311](https://github.com/nelson-lang/nelson/issues/311): betainc builtin: Incomplete beta function.

- add icon to figure

- some doxygen comments about mex functions.

- [#299](https://github.com/nelson-lang/nelson/issues/299): extends "complex" to manage sparse matrix.

## Bug Fixes:

- [#300](https://github.com/nelson-lang/nelson/issues/300): nmm returns wrong error.

## Compilation:

- Qt 5.14.2 on Windows

# 0.4.6 (2020-06-27)

- [IN PROGRESS] C MEX API:

  - extends mex function to manage interleaved complex option and c flags.

  - all C MEX API implemented, full API documentation and examples in progress.

  - mxMakeArrayReal, mxMakeArrayComplex functions.

  - mxGetImagData, mxSetImagData functions.

  - mxGetLogicals, mxIsLogicalScalar, mxIsLogicalScalarTrue functions.

  - mxGetInt8s, mxSetInt8s, mxGetComplexInt8s, mxSetComplexInt8s, mxGetUint8s, mxSetUint8s, mxGetComplexUint8s
    mxSetComplexUint8s, mxGetInt16s, mxSetInt16s, mxGetComplexInt16s, mxSetComplexInt16s, mxGetUint16s, mxGetComplexUint16s
    mxSetComplexUint16s, mxGetInt32s, mxSetInt32s, mxGetComplexInt32s, mxSetComplexInt32s, mxGetUint32s, mxSetUint32s
    mxGetComplexUint32s, mxSetComplexUint32s, mxSetUint16s, mxGetInt64s, mxSetInt64s, mxGetComplexInt64s, mxSetComplexInt64s
    mxGetUint64s, mxSetUint64s, mxGetComplexUint64s, mxSetComplexUint64s functions.

  - mxIsObject, mxIsFunctionHandle, mxIsOpaque functions.

  - mxIsInt8, mxIsInt16, mxIsInt32, mxIsInt64, mxIsUint8, mxIsUint16, mxIsUint32, mxIsUint64 functions.

  - mxCreateStringFromNChars, mxGetNChars

  - mxRemoveField, mxAddField, mxSetField, mxSetFieldByNumber, mxGetFieldNumber, mxGetFieldNameByNumber functions.

  - mexGetVariable, mexGetVariablePtr, mexPutVariable functions.

  - mexMakeArrayPersistent, mexMakeMemoryPersistent functions.

## Compilation:

- boost 1.73.0 on Windows.

- ninja-build used with github actions CI.

# 0.4.5 (2020-05-23)

- graphics object type added.

- figure builtin: creates figure.

- gcf builtin: get current figure.

- groot builtin: returns graphic root object.

- get, set, isvalid, class, fieldnames, delete, disp builtin overloaded to manage graphics objects.

- test_run: tests are sorted on all platforms.

- [IN PROGRESS] C MEX API:

  - C MEX supports build with MinGW compiler.

  - mxArray and ArrayOf conversion optimized.

  - C MEX interleaved complex support.

  - C MEX Sparse type fully supported.

  - mxGetClassName, mxSetClassName fully supported.

  - mxGetProperty, mxSetProperty fully supported (handle, graphics object, ...).

## Bug Fixes:

- [#295](https://github.com/nelson-lang/nelson/issues/295): sort did not return an wrong error message for struct.

## Compilation:

- libcurl 7.70.0 on Windows.

- cmake 3.17.2 on Windows.

- CA certificate (Wed Jan 1 04:12:10 2020 GMT)

# 0.4.4 (2020-04-29)

- lookandfeel builtin: default current application look and feel.

- clear builtin extended to clear mex functions.

- mex function used to build MEX files.

- [IN PROGRESS] MEX C API allows to access Nelson, GNU Octave and commercial software functions.

  documentation and tests will be extended in next version.

  Feedback and external tests are welcome.

## Compilation:

- MacOs X Catalina fully working.

- Ubuntu 20.04 LTS supported.

# 0.4.3 (2020-03-30)

- mean builtin: Mean elements of an array with nanflag and 'all' support.

- sum and prod optimized.

- save and load with .mat, .nh5 files support unicode filename on all platforms.

- simplify builtin default prototype (breaking change). Evaluator is no more required for builtin.

- NelsonPrint internal function added.

## Bug Fixes:

- [#287](https://github.com/nelson-lang/nelson/issues/287): Parser error message are not localized.

- [#286](https://github.com/nelson-lang/nelson/issues/286): [end] = sin(1) did not return an syntax error.

- [#284](https://github.com/nelson-lang/nelson/issues/284): Nth dimensions assignation of an empty array with 2d matrix did not work.

## Compilation:

- MATIO 1.5.17 with unicode support

- HDF5 1.12.0 support

- BISON 3.5.0

# 0.4.2 (2020-02-25)

- min, max builtin: Minimum/Maximum elements of an array with nanflag and 'all' support.

- flipud: Flip array up to down.

- fliplr: Flip array left to right.

- flip: Flip order of elements.

- flipdim: Flip array along specified dimension.

- log2 builtin: Base 2 logarithm and floating-point number dissection.

- colon operator optimized.

- faster algorithm to convert variable to different data type.

- replaces hashmap used for functions and variables.

- some few speed optimization about evaluator.

# 0.4.1 (2020-01-27)

- rework and speed optimization for times, divide, addition, subtraction operators.

- sum builtin: sum of array elements.

- linspace builtin: linearly spaced vector constructor.

- logspace builtin: logarithmically spaced vectors constructor.

- log10 builtin: Common logarithm (base 10).

- log1p builtin: log(1+x) accurately for small values of x.

- replaces dot animation by percent display about help indexing.

- html style about table simplified.

## Compilation:

- uses ASIO C++ library in place of BOOST ASIO.

- Add Qt 5.14.0 support.

- OPEN MP support added.

## Previous changelog:

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
