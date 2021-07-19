# UNRELEASED

## Features:

- macros in memory reworked to support also MEX.

- C MEX compatibility, load and build fully compatible with other softwares.

- `inmem` builtin returns names of functions, MEX-files in memory.

- `mexext` builtin returns binary MEX file-name extension.

- main function in .m no more require to be the first in file.

- checks in the .m function that other local function names are not duplicated.

- .m timestamp checked if `addpath(...,'-frozen')` is not enabled.

- function_handle reworked to have an compatible behavior.

- `struct` behavior with `function_handle`.

- `clear` reworked to support mex in memory.

- `nargin`, `nargout` behavior with mex updated.

- [#474](http://github.com/Nelson-numerical-software/nelson/issues/474): `exist`: extended to manage mex function.

- [#449](http://github.com/Nelson-numerical-software/nelson/issues/449): `conv2`: 2-D convolution and `conv`: Convolution and polynomial multiplication.

## Bug Fixes:

- [#468](http://github.com/Nelson-numerical-software/nelson/issues/468): A(':') = [] was not managed.

# 0.5.6 (2021-06-27)

BREAKING CHANGE:

## Features:

- `function ... endfunction` and `function ... end` are equivalent (increase compatibility ;).

- file extension `.m` is managed by Nelson.

  - About compatibility: scripts and functions developed with Nelson should work with other tools managing .m files. The reciprocal is not necessarily true.

  - `.m` is default and alone file extension.

- module skeleton updated to use to `.m` extension (Please update your code)

- `run` builtin can also evaluate a macro function.

- macro functions also searched in current directory.

- parser cleaned and generated with Bison 3.7.4

- `narginchk` builtin: checks number of input arguments.

- `nargoutchk` builtin: checks number of outnput arguments.

- [#448](http://github.com/Nelson-numerical-software/nelson/issues/448): data analysis module (Code refactoring).

## Bug Fixes:

- `nmm('install', existing_module_directory)` did not work as expected.

- [#451](http://github.com/Nelson-numerical-software/nelson/issues/451): var() returns an unexpected error.

## Compilation:

- [#455](http://github.com/Nelson-numerical-software/nelson/issues/455): M1 macOS apple native support. It works but some gui features can crash due to young Qt support on M1.

- Update fmt library to 8.0.

# 0.5.5 (2021-05-24)

## Features:

- Validators functions available from Nelson and C++ API (part 2):

  - `mustBeFile`,
  - `mustBeNonempty`, `mustBeNonNan`, `mustBeNonZero`, `mustBeNonSparse`,
  - `mustBeA`, `mustBeReal`, `mustBeInteger`, `mustBeNonmissing`,
  - `mustBePositive`, `mustBeNonpositive`, `mustBeNonnegative`, `mustBeNegative`,
  - `mustBeGreaterThan`, `mustBeGreaterThanOrEqual`, `mustBeLessThan`,
  - `mustBeNumericOrLogical`, `mustBeText`, `mustBeNonzeroLengthText`,
  - `mustBeMember`, `mustBeInRange`.

- `test_run` manages `SEQUENTIAL TEST REQUIRED` and `NATIVE_ARCHITECTURE TEST REQUIRED` tags.

- benchs are executed sequentialy (better bench results).

- `all`, `any` behavior with empty matrix updated.

- extends `all` to manage over all elements.

- `ismember` builtin: Array elements that are members of another array.

- [#439](http://github.com/Nelson-numerical-software/nelson/issues/439): split elementary_functions module and creates operators modules.

## Changed:

- comment symbol is '%'. others previous supported comment symbol removed.

## Bug Fixes:

- [#435](http://github.com/Nelson-numerical-software/nelson/issues/435): `maxNumCompThreads` did not return number of threads but number of cores.

## Compilation:

- Move Windows build to GitHub CI. Appveyor is no more the principal build CI for Windows.

- [#441](http://github.com/Nelson-numerical-software/nelson/issues/441): Circle CI (ArchLinux build) fixed.

- [#357](http://github.com/Nelson-numerical-software/nelson/issues/357): Curl 7.76.1 on Windows.

# 0.5.4 (2021-04-24)

## Features:

- Validators functions available from Nelson and C++ API (part 1):

  - `mustBeLogicalScalar`, `mustBeLogical`, `mustBeFloat`,
  - `mustBeFinite`, `mustBeScalarOrEmpty`, `mustBeVector`,
  - `mustBeValidVariableName`,
  - `mustBeTextScalar`, `mustBeFolder`,
  - `mustBeNumeric`.

- Functions using SIMD extensions:

  - `ceil`, `round`, `fix`, `floor`, `abs`, `conj`,
  - `exp`, `sqrt`, `log1p`, `log10`, `log`
  - `cos`, `sin`, `tan`
  - `atan2`, `acos`, `asin`
  - addition, substraction, multiplication, division vectors.

- `system` allows to run shell command execution in parallel.

- `test_run` executes on parallel process.

- extends `assert_checkerror` to check also error identifier.

- `isvector`, `isscalar` support overload.

- `isvarname` builtin: check if input is valid variable name.

- `isdir` manages string array.

- `time` returns current time as the number of seconds or nanoseconds since the epoch.

## Bug Fixes:

- [#352](http://github.com/Nelson-numerical-software/nelson/issues/352): number of input arguments checked in macro.

- [#382](http://github.com/Nelson-numerical-software/nelson/issues/382): optimize `corrcoef`.

# 0.5.3 (2021-03-24)

## Features:

- [#373](http://github.com/Nelson-numerical-software/nelson/issues/373): `sign` builtin.

- [#313](http://github.com/Nelson-numerical-software/nelson/issues/313): `atanh` builtin: inverse hyperbolic tangent.

- `MException` comes default exception in Nelson.

- `try, catch` extended to manage `MException`.

- `throw`, `throwAsCaller`, `rethrow` functions added.

- `error` extended to manage identifier.

- callstack reworks, available with `MException`.

- for loop faster > x2.

- assignment does not copy matrix.

- reworks ArrayOfVector (internal).

- C++ API nargincheck, nargoutcheck helpers added.

- rename `mexception` to `MException`

## Bug Fixes:

- [#413](http://github.com/Nelson-numerical-software/nelson/issues/413): circle CI Arch docker did not work.

- [#412](http://github.com/Nelson-numerical-software/nelson/issues/412) docker fedora 35 support.

# 0.5.2 (2021-02-27)

## Features:

- `triu` builtin: Upper triangular part of matrix.

- `tril` builtin: Lower triangular part of matrix.

- `istriu` checks if matrix is upper triangular part of matrix.

- `istril`: checks if matrix is lower triangular part of matrix.

- `isdiag`: checks if matrix is diagonal.

## Compilation:

- MacOS build uses openBLAS. lapacke included in openBLAS. No more thirdparty repository required for MacOS build.

- rename ArrayOf::getLength to ArrayOf::getElementCount method.

- rework simple assignement.

- add benchs about loop to identify existing bottleneck for next iteration.

- rework loop to prepare next iteration.

# 0.5.1 (2021-01-30)

## Features:

- `qt_version` builtin: returns the version number of Qt at run-time.

- `qt_constant` builtin: returns value of an Qt constant.

- [#374](http://github.com/Nelson-numerical-software/nelson/issues/374): `num2str` builtin: converts numbers to character array.

## Bug Fixes:

- [#388](http://github.com/Nelson-numerical-software/nelson/issues/388): Windows x64 build failed (elementary_functions module was too big).

- [#385](http://github.com/Nelson-numerical-software/nelson/issues/385): `corrcoef`, `mean`, `var`, `cov` moved in statistics module.

## Compilation:

- 0.5 family (CHANGELOG)

- Eigen 3.3.9 used.

- libsndfile 1.0.31 on Windows.

- libboost 1.75 on Windows.

- fix cirle CI build.

- [#394](http://github.com/Nelson-numerical-software/nelson/issues/394): Upgrade socket.IO dependency to v3.0.

- [#367](http://github.com/Nelson-numerical-software/nelson/issues/367): add fftw_init_threads and fftw_plan_with_nthreads to MKL wrapper for FFTW.

- [#356](http://github.com/Nelson-numerical-software/nelson/issues/356): MKL OneAPI v2021 support.

- [#355](http://github.com/Nelson-numerical-software/nelson/issues/355): Qt6 support.

- [#317](http://github.com/Nelson-numerical-software/nelson/issues/317): uses fmtlib.

## Previous changelog:

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
