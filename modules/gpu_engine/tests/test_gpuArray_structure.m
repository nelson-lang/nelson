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
A = gpuArray(single([1 2 0; 0 3 4; 0 0 5]));
S = gpuArray(single([2 1; 1 3]));
K = gpuArray(single([0 1; -1 0]));
H = gpuArray(single([2, 1 + 1i; 1 - 1i, 3]));
B = gpuArray(single([1 2 0 0; 3 4 5 0; 0 6 7 8]));
%=============================================================================
% structure tests (one device pass, logical answer on the host)
assert_istrue(istriu(A));
assert_isfalse(istril(A));
assert_isfalse(isdiag(A));
assert_istrue(isdiag(gpuArray(single(eye(3)))));
assert_istrue(isbanded(A, 0, 1));
assert_isfalse(isbanded(A, 0, 0));
assert_isequal(bandwidth(A), 0);
assert_isequal(bandwidth(B, 'upper'), 1);
[lo, up] = bandwidth(B);
assert_isequal([lo, up], [1 1]);
assert_istrue(issymmetric(S));
assert_isfalse(issymmetric(K));
assert_istrue(issymmetric(K, 'skew'));
assert_istrue(ishermitian(H));
assert_isfalse(issymmetric(H));
assert_istrue(ishermitian(S));
assert_isfalse(issymmetric(gpuArray(single([1 2 3]))));
assert_istrue(istriu(gpuArray(zeros(0, 3, 'single'))));
%=============================================================================
% structured matrices built on the device
c = gpuArray(single([1 2 3]));
r = toeplitz(c);
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), single([1 2 3; 2 1 2; 3 2 1]));
assert_isequal(gather(toeplitz(c, gpuArray(single([1 5 6 7])))), single([1 5 6 7; 2 1 5 6; 3 2 1 5]));
assert_isequal(gather(toeplitz(c, single([9 5 6]))), single([1 5 6; 2 1 5; 3 2 1]));
assert_isequal(gather(hankel(c)), single([1 2 3; 2 3 0; 3 0 0]));
assert_isequal(gather(hankel(c, single([3 4 5 6]))), single([1 2 3 4; 2 3 4 5; 3 4 5 6]));
assert_isequal(gather(vander(gpuArray(single([2 3 4])))), single([4 2 1; 9 3 1; 16 4 1]));
assert_isequal(gather(compan(c)), single([-2 -3; 1 0]));
assert_isequal(gather(blkdiag(gpuArray(single([1 2])), gpuArray(single([3; 4])))), ...
  single([1 2 0; 0 0 3; 0 0 4]));
%=============================================================================
% diag: vector to matrix and back
assert_isequal(gather(diag(c)), single([1 0 0; 0 2 0; 0 0 3]));
assert_isequal(gather(diag(c, 1)), single([0 1 0 0; 0 0 2 0; 0 0 0 3; 0 0 0 0]));
assert_isequal(size(diag(c, -2)), [5 5]);
assert_isequal(gather(diag(gpuArray(single([1 2; 3 4])))), single([1; 4]));
M = gpuArray(single([1 2 3; 4 5 6]));
assert_isequal(gather(diag(M, 1)), single([2; 6]));
assert_isequal(gather(diag(M, -1)), single(4));
assert_isequal(size(diag(gpuArray(zeros(1, 0, 'single')))), [0 0]);
assert_isequal(gather(diag(gpuArray(single([1; 2])), 0)), single([1 0; 0 2]));
%=============================================================================
