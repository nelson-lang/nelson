%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% prod of an integer array saturated its double result prematurely, and
% cumsum/cumprod lost precision (and wrapped) for 64-bit integer types
% because they accumulated in a double instead of exact integer arithmetic
%=============================================================================
% prod: default keeps a double accumulator (no premature saturation)
asserts.isequal(prod(int8([10 10 10])), 1000);
asserts.isequal(class(prod(int8([10 10 10]))), 'double');
asserts.isequal(prod(int8([100 100; 100 100]), 1), [10000 10000]);
asserts.isequal(prod(int8([100 100; 100 100]), 2), [10000; 10000]);
asserts.isequal(class(prod(int8([100 100; 100 100]), 1)), 'double');
%=============================================================================
% prod: 'native' stays in the integer class and saturates
asserts.isequal(prod(int8([10 10 10]), 'native'), int8(127));
asserts.isequal(class(prod(int8([10 10 10]), 'native')), 'int8');
asserts.isequal(prod(uint8([20 20]), 'native'), uint8(255));
%=============================================================================
% prod: negative factor times a zero factor must not display as -0
asserts.isequal(prod(int8([-5 0 3])), 0);
%=============================================================================
% sum: default of an integer array is a double (not saturated)
asserts.isequal(sum(int8([100 100 100])), 300);
asserts.isequal(class(sum(int8([1 2 3]))), 'double');
%=============================================================================
% cumsum: stays in the integer class and saturates per element
asserts.isequal(cumsum(int8([100 100 100])), int8([100 127 127]));
asserts.isequal(class(cumsum(int8([1 2 3]))), 'int8');
%=============================================================================
% cumsum: 64-bit precision and saturation (regression: double accumulator
% rounded intmax up to 2^63 and wrapped)
mx = intmax('int64');
asserts.isequal(cumsum([mx mx mx]), [mx mx mx]);
asserts.isequal(cumsum([intmin('int64') intmax('int64')]), int64([intmin('int64') -1]));
asserts.isequal(cumsum([intmax('uint64') intmax('uint64')]), [intmax('uint64') intmax('uint64')]);
%=============================================================================
% cumprod: stays in the integer class and saturates
asserts.isequal(cumprod(int8([10 10 10])), int8([10 100 127]));
asserts.isequal(class(cumprod(int8([1 2 3]))), 'int8');
%=============================================================================
% cumprod: 64-bit saturation (regression: first element wrapped to intmin)
asserts.isequal(cumprod([intmax('int64') int64(2) int64(2)]), ...
  [intmax('int64') intmax('int64') intmax('int64')]);
asserts.isequal(cumprod([intmax('uint64') intmax('uint64')]), ...
  [intmax('uint64') intmax('uint64')]);
%=============================================================================
