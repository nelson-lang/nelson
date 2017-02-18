Features:
---------

  which(function_name, '-all') without output argument display paths.

Bug Fixes:
---------

  [#34](http://github.com/Nelson-numerical-software/nelson/issues/34): Windows installer did not copy module_skeleton help files at the good place.
  
  [#30](http://github.com/Nelson-numerical-software/nelson/issues/30): Search path order for functions was wrong.


## 0.1.3 alpha (2017-02-11)

Features:
---------

  datevec updated to manage vectors, matrix.

  Add nnz, nzmax, numel functions.

  Rework searchenv to return a cell of strings.

  Update persistent variables behavior (clear functions).

  Add ceil, floor, round, fix functions.

  Add sparse support for acos, asin, atan, cos, cosh, sin, sinh, tan, tanh.

  Add website and bug reports link in help menu.

Bug Fixes:
---------

  [#28](http://github.com/Nelson-numerical-software/nelson/issues/28): Each public builtin have a help file.
  
  [#26](http://github.com/Nelson-numerical-software/nelson/issues/26): 7.853981633974482790D-01 was not correctly parsed.

  [#22](http://github.com/Nelson-numerical-software/nelson/issues/22): last output argument of 'IJV' did not return nzmax.

  [#19](http://github.com/Nelson-numerical-software/nelson/issues/19): rand & randn did not use Column-major order.

  [#17](http://github.com/Nelson-numerical-software/nelson/issues/17): 'locales' directory renamed as 'locale' (more standard).

  [#13](http://github.com/Nelson-numerical-software/nelson/issues/13): mldivide and mrdivide builtin were missing.


## 0.1.2 alpha (2017-01-01)

Bug Fixes:
---------
  [#11](http://github.com/Nelson-numerical-software/nelson/issues/11): colon constructor failed with some special values.
  
  [#10](http://github.com/Nelson-numerical-software/nelson/issues/10): module_skeleton did not build/load.
  
  [#8](http://github.com/Nelson-numerical-software/nelson/issues/8): test_nargin & test_nargout failed in Windows binary version.
  

## 0.1.1 alpha (2016-12-30)

Bug Fixes:
---------
  [#5](http://github.com/Nelson-numerical-software/nelson/issues/5): help browser did not work on some Windows

## 0.1.0 alpha (2016-12-28)
  Initial import in github of Nelson
