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
A = cat(3, [4 1; 2 3], [2 0; 1 5]);
B = cat(3, [1; 2], [3; 4]);
X3 = cat(3, [1 2; 3 4; 5 6], eye(3, 2));
%=============================================================================
% page solvers keep device results
X = pagemldivide(gpuArray(A), gpuArray(B));
assert_istrue(isgpuarray(X));
assert_isapprox(gather(X), single(pagemldivide(A, B)), 1e-5);
assert_isapprox(gather(pagemrdivide(gpuArray(permute(B, [2 1 3])), gpuArray(A))), single(pagemrdivide(permute(B, [2 1 3]), A)), 1e-5);
assert_isapprox(gather(pagesvd(gpuArray(X3))), single(pagesvd(X3)), 1e-5);
assert_isapprox(gather(pagepinv(gpuArray(X3))), single(pagepinv(X3)), 1e-5);
%=============================================================================
% minimum norm and weighted least squares
x = lsqminnorm(gpuArray([1 2 3; 4 5 6]), gpuArray([1; 2]));
assert_istrue(isgpuarray(x));
assert_isapprox(gather(x), single([-1/18; 1/9; 5/18]), 1e-5);
x = pagelsqminnorm(gpuArray(cat(3, [1 2; 2 4], [1 0; 0 2])), gpuArray(cat(3, [1; 2], [1; 1])));
assert_isapprox(gather(x(:)), single([0.2; 0.4; 1; 0.5]), 1e-5);
x = lscov(gpuArray([1 1; 1 2; 1 3]), gpuArray([1; 2; 2]), gpuArray([1; 2; 1]));
assert_istrue(isgpuarray(x));
assert_isapprox(gather(x), single([0.75; 0.5]), 1e-5);
%=============================================================================
% N-D convolution and real block diagonal form
C = convn(gpuArray(reshape(1:8, 2, 2, 2)), gpuArray(ones(2, 1, 2)), 'same');
assert_istrue(isgpuarray(C));
assert_isequal(gather(C(:))', single([14 8 22 12 11 6 15 8]));
[V, D] = eig([1 2 0; -2 1 0; 0 0 3]);
[Vr, Dr] = cdf2rdf(gpuArray(V), gpuArray(D));
[hVr, hDr] = cdf2rdf(V, D);
assert_istrue(isgpuarray(Vr));
assert_isapprox(gather(Vr), single(hVr), 1e-5);
assert_isapprox(gather(Dr), single(hDr), 1e-5);
%=============================================================================
% generalized singular values
A = [1 2; 3 4; 5 6; 7 8];
B = [1 0; 1 1];
s = gsvd(gpuArray(A), gpuArray(B));
assert_istrue(isgpuarray(s));
assert_isapprox(gather(s), single(gsvd(A, B)), 1e-5);
[U, V, X, C, S] = gsvd(gpuArray(A), gpuArray(B));
assert_istrue(norm(double(gather(U * C * X')) - A) < 1e-4);
%=============================================================================
