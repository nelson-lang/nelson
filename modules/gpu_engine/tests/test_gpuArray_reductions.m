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
a = single(rand(60, 40)) + 0.5;
A = gpuArray(a);
tol = 1e-3;
%=============================================================================
% default dim (columns, dim 1) -> 1xN
assert_isequal(size(gather(sum(A))), [1 40]);
assert_isapprox(gather(sum(A)), sum(a), tol);
assert_isapprox(gather(prod(A)), prod(a), 1e-2);
assert_isapprox(gather(mean(A)), mean(a), tol);
assert_isapprox(gather(max(A)), max(a), tol);
assert_isapprox(gather(min(A)), min(a), tol);
%=============================================================================
% explicit dim 1 and dim 2
assert_isapprox(gather(sum(A, 1)), sum(a, 1), tol);
assert_isapprox(gather(sum(A, 2)), sum(a, 2), tol);
assert_isequal(size(gather(sum(A, 2))), [60 1]);
assert_isapprox(gather(mean(A, 2)), mean(a, 2), tol);
assert_isapprox(gather(max(A, [], 2)), max(a, [], 2), tol);
assert_isapprox(gather(min(A, [], 1)), min(a, [], 1), tol);
%=============================================================================
% results stay device resident
assert_istrue(isgpuarray(sum(A)));
assert_istrue(isgpuarray(max(A, [], 2)));
%=============================================================================
% row vector -> reduces along dim 2 by default
v = single(rand(1, 30));
V = gpuArray(v);
assert_isapprox(gather(sum(V)), sum(v), tol);
assert_isequal(size(gather(sum(V))), [1 1]);
%=============================================================================
% elementwise max/min of two arrays
b = single(rand(60, 40));
B = gpuArray(b);
assert_isapprox(gather(max(A, B)), max(a, b), tol);
assert_isapprox(gather(min(A, B)), min(a, b), tol);
%=============================================================================
% max/min with scalar broadcast
assert_isapprox(gather(max(A, 0.7)), max(a, single(0.7)), tol);
%=============================================================================
% chained: normalize columns to unit sum, device resident throughout
S = sum(A);
% (broadcast scalar-per-column not supported yet; check a simpler chain)
T = sum(A .* A);
assert_isapprox(gather(T), sum(a .* a), 1e-2);
%=============================================================================
