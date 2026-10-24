%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% sum/prod of an empty array reduce the working dimension to 1 and give the
% identity (0 or 1) where the result is not empty. [] (0x0) without
% dimension (or with 'all') gives the identity, with a dimension it follows
% the rule. cumsum/cumprod keep the size of the input.
%=============================================================================
fns = {@sum, @prod};
ids = [0, 1];
for k = 1:2
  fn = fns{k};
  v = ids(k);
  asserts.isequal(fn(zeros(0, 3)), v * ones(1, 3));
  asserts.isequal(fn(zeros(0, 3), 2), zeros(0, 1));
  asserts.isequal(fn(zeros(0, 3), 3), zeros(0, 3));
  asserts.isequal(fn(zeros(3, 0)), zeros(1, 0));
  asserts.isequal(fn(zeros(3, 0), 2), v * ones(3, 1));
  asserts.isequal(fn(zeros(1, 0)), v);
  asserts.isequal(fn(zeros(0, 3, 2)), v * ones(1, 3, 2));
  asserts.isequal(fn(zeros(2, 0, 3)), zeros(1, 0, 3));
  asserts.isequal(fn(zeros(0, 3), 'omitnan'), v * ones(1, 3));
  asserts.isequal(fn(zeros(0, 3), 'all'), v);
  asserts.isequal(fn(zeros(0, 3), [1 2]), v);
  asserts.isequal(fn([]), v);
  asserts.isequal(fn([], 'all'), v);
  asserts.isequal(fn([], [1 2]), v);
  asserts.isequal(fn([], 1), zeros(1, 0));
  asserts.isequal(fn([], 2), zeros(0, 1));
  asserts.isequal(fn([], 3), zeros(0, 0));
  asserts.isequal(fn(single(zeros(0, 3))), single(v * ones(1, 3)));
  asserts.isequal(fn(single([]), 1), single(zeros(1, 0)));
  asserts.isequal(fn(false(0, 3), 'native'), logical(v * ones(1, 3)));
  asserts.isequal(fn(int8(zeros(0, 3)), 'native'), int8(v * ones(1, 3)));
  asserts.isequal(fn(int8([]), 2, 'native'), int8(zeros(0, 1)));
end
%=============================================================================
for f = {@cumsum, @cumprod}
  fn = f{1};
  asserts.isequal(fn(zeros(0, 3)), zeros(0, 3));
  asserts.isequal(fn(zeros(0, 3), 2), zeros(0, 3));
  asserts.isequal(fn(zeros(3, 0)), zeros(3, 0));
  asserts.isequal(fn(zeros(3, 0), 2), zeros(3, 0));
  asserts.isequal(fn([]), []);
  asserts.isequal(fn([], 1), []);
  asserts.isequal(fn([], 3), []);
  asserts.isequal(fn(zeros(0, 3, 2)), zeros(0, 3, 2));
  asserts.isequal(fn(zeros(0, 3), 'reverse'), zeros(0, 3));
  asserts.isequal(fn(zeros(0, 3), 'omitnan'), zeros(0, 3));
  asserts.isequal(fn(int8(zeros(0, 3))), int8(zeros(0, 3)));
  asserts.isequal(fn(single(zeros(0, 3))), single(zeros(0, 3)));
  asserts.isequal(fn(false(0, 3)), zeros(0, 3));
end
%=============================================================================
