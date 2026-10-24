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
a = single(rand(50, 40)) + 0.5;
b = single(rand(50, 40)) + 0.5;
A = gpuArray(a);
B = gpuArray(b);
tol = 1e-5;
%=============================================================================
% binary operators
assert_isapprox(gather(A + B), a + b, tol);
assert_isapprox(gather(A - B), a - b, tol);
assert_isapprox(gather(A .* B), a .* b, tol);
assert_isapprox(gather(A ./ B), a ./ b, tol);
assert_isapprox(gather(A .\ B), a .\ b, tol);
assert_isapprox(gather(A .^ B), a .^ b, 1e-4);
%=============================================================================
% each result is device resident
assert_istrue(isgpuarray(A + B));
assert_istrue(isgpuarray(A .^ B));
%=============================================================================
% scalar broadcast, both directions (non-commutative ops matter here)
assert_isapprox(gather(A - 1), a - 1, tol);
assert_isapprox(gather(10 - A), 10 - a, tol);
assert_isapprox(gather(A ./ 2), a ./ 2, tol);
assert_isapprox(gather(2 ./ A), 2 ./ a, tol);
%=============================================================================
% unary operators / math functions
assert_isapprox(gather(-A), -a, tol);
assert_isapprox(gather(abs(-A)), a, tol);
assert_isapprox(gather(sqrt(A)), sqrt(a), tol);
assert_isapprox(gather(exp(A)), exp(a), 1e-4);
assert_isapprox(gather(log(A)), log(a), tol);
assert_isapprox(gather(sin(A)), sin(a), tol);
assert_isapprox(gather(cos(A)), cos(a), tol);
assert_isapprox(gather(tan(A)), tan(a), 1e-4);
%=============================================================================
% chained device-resident expression (no intermediate gather)
R = sqrt(A .* A + B .* B);
assert_isapprox(gather(R), sqrt(a .* a + b .* b), 1e-4);
%=============================================================================
