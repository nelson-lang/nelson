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
g = gpuArray(1:6);
R = trimdata(g, 4, 'Side', 'both');
assert_istrue(isgpuarray(R));
assert_isequal(gather(R), single(2:5));
R = paddata(g, 9, 'Pattern', 'reflect');
assert_istrue(isgpuarray(R));
assert_isequal(gather(R), single([1 2 3 4 5 6 5 4 3]));
assert_isequal(gather(paddata(g, 8, 'FillValue', 7)), single([1:6 7 7]));
R = inpolygon(gpuArray([0.5 2]), gpuArray([0.5 2]), [0 1 1 0], [0 0 1 1]);
assert_isequal(gather(R), [true false]);
R = createArray(2, 3, 'like', g, 'FillValue', 4);
assert_istrue(isgpuarray(R));
assert_isequal(gather(R), single(4 * ones(2, 3)));
assert_isequal(classUnderlying(gpuArray(single(1))), 'single');
assert_istrue(isaUnderlying(gpuArray(single(1)), 'float'));
assert_isfalse(isaUnderlying(gpuArray(single(1)), 'integer'));
assert_istrue(isUnderlyingType(gpuArray(single(1)), 'numeric'));
assert_isequal(getrangefromclass(gpuArray(single(1))), [0 1]);
n = 20;
e = ones(n, 1);
A = full(spdiags([-1 * e, 2.5 * e, -0.5 * e], -1:1, n, n));
b = (1:n).';
[x, flag] = tfqmr(gpuArray(A), gpuArray(b), 1e-5, 30);
assert_istrue(isgpuarray(x));
assert_isequal(flag, 0);
assert_isapprox(gather(x), single(A \ b), 1e-3);
%=============================================================================
