%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% CPU-fallback overloads (\, inv, norm, eig, svd, fft): computed on host.
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
A = gpuArray(single([4 1; 2 3]));
b = gpuArray(single([1; 2]));
tol = 1e-4;
%=============================================================================
% linear solve returns a gpuArray
x = A \ b;
assert_istrue(isgpuarray(x));
assert_isapprox(gather(x), single([4 1; 2 3]) \ single([1; 2]), tol);
%=============================================================================
% inv / norm return gpuArray
assert_istrue(isgpuarray(inv(A)));
assert_isapprox(gather(inv(A)), inv(single([4 1; 2 3])), tol);
assert_istrue(isgpuarray(norm(A)));
assert_isapprox(gather(norm(A)), norm(single([4 1; 2 3])), tol);
%=============================================================================
% eig / svd / fft return host results in double precision (may be complex)
e = eig(A);
assert_isfalse(isgpuarray(e));
assert_isapprox(sort(real(e)), sort(real(eig(double([4 1; 2 3])))), tol);
%=============================================================================
s = svd(A);
assert_isfalse(isgpuarray(s));
assert_isapprox(s, svd(double([4 1; 2 3])), tol);
%=============================================================================
f = fft(gpuArray(single([1 2 3 4])));
assert_isfalse(isgpuarray(f));
assert_isapprox(f, fft(double([1 2 3 4])), tol);
%=============================================================================
% matrix functions expm / logm / sqrtm: host fallback, re-wrapped as gpuArray
M = single([4 2; 1 3]);
GM = gpuArray(M);
E = expm(GM);
assert_istrue(isgpuarray(E));
assert_isapprox(double(gather(E)), double(expm(M)), 1e-3);
% logm(expm(M)) recovers M
assert_isapprox(double(gather(logm(expm(GM)))), double(M), 1e-2);
% sqrtm squared recovers the (diagonal) matrix
D = single([2 0; 0 3]);
R = sqrtm(gpuArray(D));
assert_isapprox(double(gather(R * R)), double(D), 1e-3);
% a complex result stays a gpuArray (device stores complex)
LN = logm(gpuArray(single([-1 0; 0 -2])));
assert_istrue(isgpuarray(LN));
assert_isapprox(double(gather(LN)), double(logm(single([-1 0; 0 -2]))), 1e-2);
%=============================================================================
