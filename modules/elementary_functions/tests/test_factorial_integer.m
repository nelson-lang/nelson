%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% 64 bit integers: exact values, saturation beyond
asserts.isequal(factorial(uint64(20)), uint64(2432902008176640000));
asserts.isequal(factorial(uint64(21)), intmax('uint64'));
asserts.isequal(factorial(int64(20)), int64(2432902008176640000));
asserts.isequal(factorial(int64(21)), intmax('int64'));
asserts.isequal(factorial(uint64([0 1 5 20])), uint64([1 1 120 2432902008176640000]));
% small integer classes saturate
asserts.isequal(factorial(int8(6)), int8(127));
asserts.isequal(factorial(uint8(5)), uint8(120));
asserts.isequal(factorial(uint8([1 2 3 4 5])), uint8([1 2 6 24 120]));
asserts.isequal(factorial(int16(7)), int16(5040));
asserts.isequal(factorial(uint32(12)), uint32(479001600));
% double stays exact
asserts.isequal(factorial(0:10), [1 1 2 6 24 120 720 5040 40320 362880 3628800]);
asserts.isequal(factorial(170), 7.257415615307994e+306);
asserts.isequal(factorial(171), Inf);
% single
R = factorial(single(10));
asserts.isequal(R, single(3628800));
asserts.isequal(class(R), 'single');
% error cases
asserts.checkerror('factorial(-1)', sprintf(_('#%d argument: must be real positive integer values.'), 1));
asserts.checkerror('factorial(2.5)', sprintf(_('#%d argument: must be real positive integer values.'), 1));
%=============================================================================
