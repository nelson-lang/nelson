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
% N-D reduction (3-D array), reduce along each dimension
a = single(reshape(1:60, 3, 4, 5)) + 0.25;
A = gpuArray(a);
tol = 1e-2;
%=============================================================================
assert_isequal(size(gather(sum(A, 1))), [1 4 5]);
assert_isapprox(gather(sum(A, 1)), sum(a, 1), tol);
assert_isequal(size(gather(sum(A, 2))), [3 1 5]);
assert_isapprox(gather(sum(A, 2)), sum(a, 2), tol);
assert_isequal(size(gather(sum(A, 3))), [3 4]);
assert_isapprox(gather(sum(A, 3)), sum(a, 3), tol);
%=============================================================================
% default dim on 3-D reduces along dim 1
assert_isapprox(gather(sum(A)), sum(a), tol);
assert_isapprox(gather(mean(A, 3)), mean(a, 3), tol);
assert_isapprox(gather(max(A, [], 2)), max(a, [], 2), tol);
%=============================================================================
% max/min second output (indices)
m = single([3 9 1; 7 2 8]);
M = gpuArray(m);
[vals, idx] = max(M);
assert_isapprox(gather(vals), max(m), tol);
[refv, refi] = max(m);
assert_isequal(gather(idx), single(refi));
%=============================================================================
[vals2, idx2] = min(M, [], 2);
[refv2, refi2] = min(m, [], 2);
assert_isapprox(gather(vals2), refv2, tol);
assert_isequal(gather(idx2), single(refi2));
%=============================================================================
% index output is device resident
assert_istrue(isgpuarray(idx));
%=============================================================================
