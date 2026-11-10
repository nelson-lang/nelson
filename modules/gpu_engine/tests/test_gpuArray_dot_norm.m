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
tol = 1e-5;
%=============================================================================
% dot: vectors (orientation-independent scalar), matrices (column dot products)
a = single([1 2 3]);
b = single([4 5 6]);
assert_isapprox(gather(dot(gpuArray(a), gpuArray(b))), dot(a, b), tol);
ac = single([1; 2; 3]);
bc = single([4; 5; 6]);
assert_isapprox(gather(dot(gpuArray(ac), gpuArray(bc))), dot(ac, bc), tol);
M = single([1 2; 3 4]);
N = single([5 6; 7 8]);
assert_isapprox(gather(dot(gpuArray(M), gpuArray(N))), dot(M, N), tol);
assert_isapprox(gather(dot(gpuArray(M), gpuArray(N), 2)), dot(M, N, 2), tol);
assert_istrue(isgpuarray(dot(gpuArray(a), gpuArray(b))));
%=============================================================================
% dot uses conj(a) for complex inputs
za = single([1 + 2i, 3 - 1i, 0 + 4i]);
zb = single([2 - 1i, 1 + 1i, 5 + 0i]);
assert_isapprox(gather(dot(gpuArray(za), gpuArray(zb))), dot(za, zb), 1e-4);
%=============================================================================
% norm: vector 2/1/inf/-inf and general p (device), matrix 2-norm (host fallback)
v = single([3 -4 0 12]);
assert_isapprox(gather(norm(gpuArray(v))), norm(v), tol);
assert_isapprox(gather(norm(gpuArray(v), 2)), norm(v, 2), tol);
assert_isapprox(gather(norm(gpuArray(v), 1)), norm(v, 1), tol);
assert_isapprox(gather(norm(gpuArray(v), inf)), norm(v, inf), tol);
assert_isapprox(gather(norm(gpuArray(v), -inf)), norm(v, -inf), tol);
assert_isapprox(gather(norm(gpuArray(v), 3)), norm(v, 3), 1e-4);
%=============================================================================
% Frobenius norm (device) and matrix 2-norm (host fallback), both device-resident
assert_isapprox(gather(norm(gpuArray(M), 'fro')), norm(M, 'fro'), tol);
assert_isapprox(gather(norm(gpuArray(M))), norm(M), 1e-4);
assert_istrue(isgpuarray(norm(gpuArray(v))));
assert_istrue(isgpuarray(norm(gpuArray(M))));
%=============================================================================
% norm of a complex vector
assert_isapprox(gather(norm(gpuArray(za))), norm(za), 1e-4);
%=============================================================================
% norms computed in one pass on the device, without overflow
M = single([1 -2; 3 4]);
assert_isequal(gather(norm(gpuArray(M), 1)), single(6));
assert_isequal(gather(norm(gpuArray(M), Inf)), single(7));
assert_isapprox(gather(norm(gpuArray(M), 'fro')), norm(M, 'fro'), 1e-6);
assert_isapprox(gather(norm(gpuArray(single([1e30 1e30])))), single(sqrt(2) * 1e30), 1e-6);
assert_istrue(isnan(gather(norm(gpuArray(single([1 NaN]))))));
assert_isequal(gather(norm(gpuArray(single([1 Inf])))), single(Inf));
assert_isapprox(gather(norm(gpuArray(single([3 + 4i, 1 - 1i])))), norm(single([3 + 4i, 1 - 1i])), 1e-6);
assert_isapprox(gather(norm(gpuArray(single([3 4 0 -12])), 3)), norm(single([3 4 0 -12]), 3), 1e-6);
assert_isequal(gather(norm(gpuArray(single([3 4 0 -12])), -Inf)), single(0));
assert_isapprox(gather(vecnorm(gpuArray(M))), vecnorm(M), 1e-6);
assert_isequal(gather(vecnorm(gpuArray(M), 1, 2)), single([3; 7]));
assert_isequal(gather(vecnorm(gpuArray(M), Inf)), single([3 4]));
v = rand(1e6, 1, 'single');
assert_isapprox(double(gather(norm(gpuArray(v)))), norm(double(v)), 1e-6);
%=============================================================================
