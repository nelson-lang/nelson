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
% Additional elementwise math functions match the host result (device
% transcendentals are single precision, hence the loose tolerances).
a = single([0.2 0.5 0.9 1.5]);
A = gpuArray(a);
assert_isapprox(gather(sinh(A)), sinh(a), 1e-4);
assert_isapprox(gather(cosh(A)), cosh(a), 1e-4);
assert_isapprox(gather(tanh(A)), tanh(a), 1e-4);
assert_isapprox(gather(atan(A)), atan(a), 1e-4);
assert_isapprox(gather(log2(A)), log2(a), 1e-3);
assert_isapprox(gather(log10(A)), log10(a), 1e-3);
%=============================================================================
% asin / acos on a valid domain (|x| <= 1); GPU inverse trig is less precise
b = single([-0.7 0.1 0.4 0.9]);
B = gpuArray(b);
assert_isapprox(gather(asin(B)), asin(b), 1e-3);
assert_isapprox(gather(acos(B)), acos(b), 1e-3);
%=============================================================================
% binary atan2 (sign of both arguments) and hypot
y = single([3 -3 1 0]);
x = single([4 4 -1 -2]);
Y = gpuArray(y);
X = gpuArray(x);
assert_isapprox(gather(atan2(Y, X)), atan2(y, x), 1e-4);
assert_isapprox(gather(hypot(Y, X)), hypot(y, x), 1e-4);
%=============================================================================
% results stay device resident
assert_istrue(isgpuarray(tanh(A)));
assert_istrue(isgpuarray(hypot(Y, X)));
%=============================================================================
% predicates isnan / isinf / isfinite (logical result)
p = single([1 Inf -Inf NaN 0 -3.5]);
P = gpuArray(p);
assert_isequal(gather(isnan(P)), isnan(p));
assert_isequal(gather(isinf(P)), isinf(p));
assert_isequal(gather(isfinite(P)), isfinite(p));
assert_isequal(class(gather(isnan(P))), 'logical');
% an integer array has no NaN/Inf
assert_isequal(gather(isnan(gpuArray(int8([1 2 -3])))), [false false false]);
assert_isequal(gather(isinf(gpuArray(int32([1 2 -3])))), [false false false]);
assert_isequal(gather(isfinite(gpuArray(int32([1 2 -3])))), [true true true]);
%=============================================================================
% reciprocal trig, inverse hyperbolic, exp/log variants and angle conversions
c = single([0.3 0.7 1.2 2.1]);
Cc = gpuArray(c);
assert_isapprox(gather(expm1(Cc)), expm1(c), 1e-4);
assert_isapprox(gather(log1p(Cc)), log1p(c), 1e-4);
assert_isapprox(gather(pow2(Cc)), pow2(c), 1e-4);
assert_isapprox(gather(sec(Cc)), sec(c), 1e-3);
assert_isapprox(gather(csc(Cc)), csc(c), 1e-4);
assert_isapprox(gather(cot(Cc)), cot(c), 1e-4);
assert_isapprox(gather(sech(Cc)), sech(c), 1e-4);
assert_isapprox(gather(csch(Cc)), csch(c), 1e-4);
assert_isapprox(gather(coth(Cc)), coth(c), 1e-4);
assert_isapprox(gather(asinh(Cc)), asinh(c), 1e-4);
assert_isapprox(gather(deg2rad(Cc)), deg2rad(c), 1e-4);
assert_isapprox(gather(rad2deg(Cc)), rad2deg(c), 1e-3);
%=============================================================================
% inverse reciprocal trig on |x| >= 1, inverse hyperbolic on their domains
d = single([1.5 2.0 3.0 5.0]);
Dd = gpuArray(d);
assert_isapprox(gather(asec(Dd)), asec(d), 1e-3);
assert_isapprox(gather(acsc(Dd)), acsc(d), 1e-3);
assert_isapprox(gather(acot(Dd)), acot(d), 1e-3);
assert_isapprox(gather(acosh(Dd)), acosh(d), 1e-4);
e = single([-0.6 -0.2 0.3 0.8]);
Ee = gpuArray(e);
assert_isapprox(gather(atanh(Ee)), atanh(e), 1e-4);
% results stay device resident
assert_istrue(isgpuarray(expm1(Cc)));
%=============================================================================
