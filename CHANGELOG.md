# 0.5.XX (2021-01-XX)

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
