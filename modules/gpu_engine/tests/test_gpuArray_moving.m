%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
x = gpuArray(single([4 8 6 -1 -2 -3 -1 3 4 5]));
y = gpuArray(single([1 NaN 3 4 5]));
tol = 1e-5;
%=============================================================================
% windows: odd centred, even one more element before, [before after]
r = movsum(x, 3);
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), single([12 18 13 3 -6 -6 -1 6 12 9]));
assert_isequal(gather(movsum(x, 4)), single([12 18 17 11 0 -7 -3 3 11 12]));
assert_isequal(gather(movsum(x, [2 0])), single([4 12 18 13 3 -6 -6 -1 6 12]));
assert_isequal(gather(movsum(x, [0 1])), single([12 14 5 -3 -5 -4 2 7 9 5]));
assert_isequal(gather(movsum(x, 100)), 23 * ones(1, 10, 'single'));
assert_isapprox(gather(movmean(x, 3)), single([6 6 13 / 3 1 -2 -2 -1 / 3 2 4 4.5]), tol);
assert_isapprox(gather(movmean(x, 4)), single([6 6 4.25 2.75 0 -1.75 -0.75 0.75 2.75 4]), tol);
assert_isequal(gather(movmax(x, 3)), single([8 8 8 6 -1 -1 3 4 5 5]));
assert_isequal(gather(movmin(x, 4)), single([4 4 -1 -2 -3 -3 -3 -3 -1 3]));
assert_isequal(gather(movprod(x, 3)), single([32 192 -48 12 -6 -6 9 -12 60 20]));
%=============================================================================
% variance and standard deviation (normalized by N - 1 by default)
assert_isapprox(gather(movvar(x, 3)), single([8 4 67 / 3 19 1 1 28 / 3 7 1 0.5]), tol);
assert_isapprox(gather(movvar(x, 3, 1)), ...
  single([4 8 / 3 134 / 9 38 / 3 2 / 3 2 / 3 56 / 9 14 / 3 2 / 3 0.25]), tol);
assert_isapprox(gather(movstd(x, 3)), sqrt(single([8 4 67 / 3 19 1 1 28 / 3 7 1 0.5])), tol);
assert_isequal(gather(movstd(x, 1)), zeros(1, 10, 'single'));
%=============================================================================
% NaN: kept by sum/mean/prod/var, skipped by max/min and with 'omitnan'
r = gather(movsum(y, 3));
assert_isequal(isnan(r), [true true true false false]);
assert_isequal(r(4:5), single([12 9]));
assert_isequal(gather(movsum(y, 3, 'omitnan')), single([1 4 7 12 9]));
assert_isequal(gather(movmax(y, 3)), single([1 3 4 5 5]));
assert_isequal(gather(movmin(y, 2)), single([1 1 3 3 4]));
assert_isequal(gather(movmean(y, 3, 'omitnan')), single([1 2 3.5 4 4.5]));
assert_isequal(gather(movvar(y, 3, 'omitnan')), single([0 2 0.5 1 0.5]));
assert_isequal(gather(movprod(y, 3, 'omitnan')), single([1 3 12 60 20]));
%=============================================================================
% along a chosen dimension
M = gpuArray(single([1 2; 3 4; 5 6]));
assert_isequal(gather(movsum(M, 2)), single([1 2; 4 6; 8 10]));
assert_isequal(gather(movsum(M, 2, 2)), single([1 3; 3 7; 5 11]));
%=============================================================================
