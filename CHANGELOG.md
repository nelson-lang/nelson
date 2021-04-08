# 0.5.4 (UNRELEASED)

## Features:

- Validators functions available from Nelson and C++ API

  - `mustBeLogicalScalar`, `mustBeLogical`,
  - `mustBeFinite`, `mustBeScalarOrEmpty`,
  - `mustBeValidVariableName`

- extends `assert_checkerror` to check also error identifier.

- `isvector`, `isscalar` support overload.

- `isvarname` builtin: check if input is valid variable name.

- Functions using SIMD extensions:

  - `ceil`, `round`, `fix`, `floor`, `abs`, `conj`,
  - `exp`, `sqrt`, `log1p`, `log10`, `log`
  - `cos`, `sin`, `tan`
  - `atan2`, `acos`, `asin`
  - addition, substraction, multiplication, division vectors.

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
