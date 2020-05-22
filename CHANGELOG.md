# 0.4.5 (2020-05-XX)

* graphic object type added.

* figure builtin: creates figure.

* gcf builtin: get current figure.

* groot builtin: returns graphic root object.

* get, set, isvalid, class, fieldnames, delete, disp builtin overloaded to manage graphic objects.

* test_run: tests are sorted on all platforms.

* [IN PROGRESS] C MEX API:

  * C MEX supports build with MinGW compiler.

  * mxArray and ArrayOf conversion optimized.

  * C MEX interleaved complex support.

  * C MEX Sparse type fully supported.


Bug Fixes:
---------

  * [#295](http://github.com/Nelson-numerical-software/nelson/issues/295): sort did not return an wrong error message for struct.


Compilation:
------------

* libcurl 7.70.0 on Windows.

* cmake 3.17.2 on Windows.

* CA certificate (Wed Jan 1 04:12:10 2020 GMT)


# 0.4.4 (2020-04-29)

* lookandfeel builtin: default current application look and feel.

* clear builtin extended to clear mex functions.

* mex function used to build MEX files.

* [IN PROGRESS] MEX C API allows to access Nelson, GNU Octave and commercial software functions.

  documentation and tests will be extended in next version.

  Feedback and external tests are welcome.

Compilation:
------------

* MacOs X Catalina fully working.

* Ubuntu 20.04 LTS supported.


# 0.4.3 (2020-03-30)

* mean builtin: Mean elements of an array with nanflag and 'all' support.

* sum and prod optimized.

* save and load with .mat, .nh5 files support unicode filename on all platforms.

* simplify builtin default prototype (breaking change). Evaluator is no more required for builtin.

* NelsonPrint internal function added.


Bug Fixes:
---------

  * [#287](http://github.com/Nelson-numerical-software/nelson/issues/287): Parser error message are not localized.

  * [#286](http://github.com/Nelson-numerical-software/nelson/issues/286): [end] = sin(1) did not return an syntax error.

  * [#284](http://github.com/Nelson-numerical-software/nelson/issues/284): Nth dimensions assignation of an empty array with 2d matrix did not work.


Compilation:
------------

* MATIO 1.5.17 with unicode support

* HDF5 1.12.0 support

* BISON 3.5.0



# 0.4.2 (2020-02-25)

* min, max builtin: Minimum/Maximum elements of an array with nanflag and 'all' support.

* flipud: Flip array up to down.

* fliplr: Flip array left to right.

* flip: Flip order of elements.

* flipdim: Flip array along specified dimension.

* log2 builtin: Base 2 logarithm and floating-point number dissection.

* colon operator optimized.

* faster algorithm to convert variable to different data type.

* replaces hashmap used for functions and variables.

* some few speed optimization about evaluator.


# 0.4.1 (2020-01-27)

* rework and speed optimization for times, divide, addition, subtraction operators.

* sum builtin: sum of array elements.

* linspace builtin: linearly spaced vector constructor.

* logspace builtin: logarithmically spaced vectors constructor.

* log10 builtin: Common logarithm (base 10).

* log1p builtin: log(1+x) accurately for small values of x.

* replaces dot animation by percent display about help indexing.

* html style about table simplified.


Compilation:
------------

* uses ASIO C++ library in place of BOOST ASIO.

* Add Qt 5.14.0 support.

* OPEN MP support added.


Previous changelog:
---------

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
