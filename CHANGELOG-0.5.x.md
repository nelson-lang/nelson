# 0.5.12 (2021-12-31)

## Added

- `hankel` function: Hankel matrix.
- `factor` function: Prime factors.
- `primes` function: Prime numbers less than or equal to input value.
- `isrow` function: Determine whether input is row vector.
- `iscolumn` function: Determine whether input is column vector.

## Fixed

- [#544](http://github.com/nelson-lang/nelson/issues/544): add `folder` fieldname to `dir` output.

- [#541](http://github.com/nelson-lang/nelson/issues/541): common class between two elements for operators, horzcat and vertcat.

## Compilation

- Boost 1.78 support (default on Windows).

- CMake 3.22.1 (on Windows).

# 0.5.11 (2021-11-26)

## Added

- `hilb` function: Hilbert matrix.
- `invhilb` function: Inverse of Hilbert matrix.
- `cond` function: Condition number for inversion.
- `rank` function: Rank of matrix.
- `ismatrix` function: Determines whether input is matrix.
- `squeeze` function: Removes dimensions of length 1.
- `speye` function: Sparse identity matrix.
- `randperm` function: Random permutation.
- `cat` function: Concatenate arrays.
- `SECURITY.md` file as recommended by Github.

## Fixed

- `vercat`, `horzcat` returns an empty array whose size is equal to the output size as when the inputs are nonempty.
- [#533](http://github.com/nelson-lang/nelson/issues/533): `find` with one lhs did not return expected result with complex.
- [#536](http://github.com/nelson-lang/nelson/issues/536): `test_websave_3` failed randomly due to distant server.

# 0.5.10 (2021-10-30)

- Polynomial functions:

  - `poly`: Polynomial with specified roots or characteristic polynomial.
  - `roots`: Polynomial roots.
  - `polyval`: Polynomial evaluation.
  - `polyvalm`: Matrix polynomial evaluation.
  - `polyint`: Polynomial integration.
  - `polyfit`: Polynomial curve fitting.
  - `polyder`: Polynomial differentiation.

- `pinv`: Moore-Penrose pseudoinverse.

- [#520](http://github.com/nelson-lang/nelson/issues/520): `inputname` get variable name of function input.

- [#525](http://github.com/nelson-lang/nelson/issues/525): use [`fast_float`](https://github.com/fastfloat/fast_float) library to parse numbers .

- [#528](http://github.com/nelson-lang/nelson/issues/528): Assignment in cell did not work in this case `[c{:}] = ind2sub (dv, i)`

- [#534](http://github.com/nelson-lang/nelson/issues/534): `diag(ones(0, 1), -1)` did not return zero as result.

# 0.5.9 (2021-09-29)

- `leapyear` function: determine leap year.

- `meshgrid` function: Cartesian rectangular grid in 2-D or 3-D.

- `sub2ind` function: linear index to matrix subscript values.

- `ind2sub` function: matrix subscript values to linear index.

- [#518](http://github.com/nelson-lang/nelson/issues/518): `isStringScalar` checks if input is string array with one element.

- [#516](http://github.com/nelson-lang/nelson/issues/516): `ind = 2; ind(false)` logical extraction on scalar should return empty matrix.

- [#514](http://github.com/nelson-lang/nelson/issues/514): `C{3} = 4` should create a cell with good dimensions.

- [#512](http://github.com/nelson-lang/nelson/issues/512): Assign must not change left assign type when it is possible.

- [#509](http://github.com/nelson-lang/nelson/issues/509): horzcat vertcat generic support for class object.

- [#508](http://github.com/nelson-lang/nelson/issues/508): Change default seed for 'rand' with Mersenne Twister algo.

- [#506](http://github.com/nelson-lang/nelson/issues/506): Modernize windows installer style.

## Compilation:

- [#496](http://github.com/nelson-lang/nelson/issues/496): Eigen 3.4 used.

- [#503](http://github.com/nelson-lang/nelson/issues/503): Boost 1.77 support (default on Windows).

# 0.5.8 (2021-08-25)

## Features:

- `test_run` displays errors with file and line number.

- CTRL-C throws an error.

- some warnings detected by LGTM or VS fixed.

- allows .m file empty to be called.

- [#477](http://github.com/nelson-lang/nelson/issues/477): update files watcher algo.

- [#489](http://github.com/nelson-lang/nelson/issues/489): display builtin and associated overload.

- [#490](http://github.com/nelson-lang/nelson/issues/490): update default prompt.

## Bug Fixes:

- [#480](http://github.com/nelson-lang/nelson/issues/480): publisher name updated for windows installer.

- [#483](http://github.com/nelson-lang/nelson/issues/483): extern modules no more build if boost not available.

- [#486](http://github.com/nelson-lang/nelson/issues/486): `inmem` help was missing.

- [#464](http://github.com/nelson-lang/nelson/issues/464): simplify macos build (catalina & BigSur support only).

- [#499](http://github.com/nelson-lang/nelson/issues/499): rename `getContentAsUnsignedInt64Scalar` to `getContentAsUnsignedInteger64Scalar`.

- [#495](http://github.com/nelson-lang/nelson/issues/495): some mtimes call failed.

# 0.5.7 (2021-07-24)

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

- [#474](http://github.com/nelson-lang/nelson/issues/474): `exist`: extended to manage mex function.

- [#449](http://github.com/nelson-lang/nelson/issues/449): `conv2`: 2-D convolution and `conv`: Convolution and polynomial multiplication.

## Bug Fixes:

- [#468](http://github.com/nelson-lang/nelson/issues/468): A(':') = [] was not managed.

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

- [#448](http://github.com/nelson-lang/nelson/issues/448): data analysis module (Code refactoring).

## Bug Fixes:

- `nmm('install', existing_module_directory)` did not work as expected.

- [#451](http://github.com/nelson-lang/nelson/issues/451): var() returns an unexpected error.

## Compilation:

- [#455](http://github.com/nelson-lang/nelson/issues/455): M1 macOS apple native support. It works but some gui features can crash due to young Qt support on M1.

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

- [#439](http://github.com/nelson-lang/nelson/issues/439): split elementary_functions module and creates operators modules.

## Changed:

- comment symbol is '%'. others previous supported comment symbol removed.

## Bug Fixes:

- [#435](http://github.com/nelson-lang/nelson/issues/435): `maxNumCompThreads` did not return number of threads but number of cores.

## Compilation:

- Move Windows build to GitHub CI. Appveyor is no more the principal build CI for Windows.

- [#441](http://github.com/nelson-lang/nelson/issues/441): Circle CI (ArchLinux build) fixed.

- [#357](http://github.com/nelson-lang/nelson/issues/357): Curl 7.76.1 on Windows.

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

- [#352](http://github.com/nelson-lang/nelson/issues/352): number of input arguments checked in macro.

- [#382](http://github.com/nelson-lang/nelson/issues/382): optimize `corrcoef`.

# 0.5.3 (2021-03-24)

## Features:

- [#373](http://github.com/nelson-lang/nelson/issues/373): `sign` builtin.

- [#313](http://github.com/nelson-lang/nelson/issues/313): `atanh` builtin: inverse hyperbolic tangent.

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

- [#413](http://github.com/nelson-lang/nelson/issues/413): circle CI Arch docker did not work.

- [#412](http://github.com/nelson-lang/nelson/issues/412) docker fedora 35 support.

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

- [#374](http://github.com/nelson-lang/nelson/issues/374): `num2str` builtin: converts numbers to character array.

## Bug Fixes:

- [#388](http://github.com/nelson-lang/nelson/issues/388): Windows x64 build failed (elementary_functions module was too big).

- [#385](http://github.com/nelson-lang/nelson/issues/385): `corrcoef`, `mean`, `var`, `cov` moved in statistics module.

## Compilation:

- 0.5 family (CHANGELOG)

- Eigen 3.3.9 used.

- libsndfile 1.0.31 on Windows.

- libboost 1.75 on Windows.

- fix cirle CI build.

- [#394](http://github.com/nelson-lang/nelson/issues/394): Upgrade socket.IO dependency to v3.0.

- [#367](http://github.com/nelson-lang/nelson/issues/367): add fftw_init_threads and fftw_plan_with_nthreads to MKL wrapper for FFTW.

- [#356](http://github.com/nelson-lang/nelson/issues/356): MKL OneAPI v2021 support.

- [#355](http://github.com/nelson-lang/nelson/issues/355): Qt6 support.

- [#317](http://github.com/nelson-lang/nelson/issues/317): uses fmtlib.

## Previous changelog:

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
