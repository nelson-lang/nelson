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
% element-wise math computed on the device, same accuracy on every backend
x = single(linspace(-6, 6, 241));
names = {'sin', 'cos', 'atan', 'sinh', 'cosh', 'tanh', 'asinh', 'expm1', 'sind', 'cosd', 'atand', ...
         'sinpi', 'cospi', 'erf', 'erfc', 'nextpow2'};
for k = 1:numel(names)
  f = names{k};
  g = feval(f, gpuArray(x));
  assert_istrue(isgpuarray(g));
  ref = feval(f, double(x));
  err = max(abs(double(gather(g)) - ref) ./ max(1, abs(ref)));
  assert_istrue(err < 2e-6, [f, ' is not accurate enough']);
end
u = single(linspace(-0.999, 0.999, 201));
for f = {'asin', 'acos', 'atanh', 'erfinv', 'asind', 'acosd'}
  ref = feval(f{1}, double(u));
  err = max(abs(double(gather(feval(f{1}, gpuArray(u)))) - ref) ./ max(1, abs(ref)));
  assert_istrue(err < 2e-6, [f{1}, ' is not accurate enough']);
end
p = single(linspace(0.1, 30, 150));
for f = {'gamma', 'gammaln', 'log1p', 'sqrt', 'log', 'log2', 'log10'}
  ref = feval(f{1}, double(p));
  err = max(abs(double(gather(feval(f{1}, gpuArray(p)))) - ref) ./ max(1, abs(ref)));
  assert_istrue(err < 2e-5, [f{1}, ' is not accurate enough']);
end
%=============================================================================
% exact values
assert_isequal(gather(sind(gpuArray(single([0 30 90 180 270 360])))), single([0 0.5 1 0 -1 0]));
assert_isequal(gather(cosd(gpuArray(single([0 90 180 270])))), single([1 0 -1 0]));
assert_isequal(gather(sinpi(gpuArray(single([0 0.5 1 1.5 2])))), single([0 1 0 -1 0]));
assert_isequal(gather(factorial(gpuArray(single([0 1 5 10])))), single([1 1 120 3628800]));
assert_isequal(gather(nextpow2(gpuArray(single([0 1 2 3 1024 1025])))), single([0 0 1 2 10 11]));
assert_isequal(gather(expm1(gpuArray(single(1e-10)))), single(1e-10));
assert_isequal(gather(log1p(gpuArray(single(1e-10)))), single(1e-10));
v = single(rand(1, 6) + 0.1);
assert_isequal(gather(mod(gpuArray(v), gpuArray(v))), zeros(1, 6, 'single'));
assert_isequal(gather(rem(gpuArray(v), gpuArray(v))), zeros(1, 6, 'single'));
assert_isequal(gather(nthroot(gpuArray(single([-27 8 32])), single([3 3 5]))), single([-3 2 2]));
assert_isequal(gather(realpow(gpuArray(single([-2 3])), 3)), single([-8 27]));
%=============================================================================
% signed zeros and infinities
z = gather(cscd(gpuArray(single([0 -0]))));
assert_isequal(z, single([Inf -Inf]));
assert_isequal(gather(coth(gpuArray(single(-0)))), single(-Inf));
assert_isequal(gather(gamma(gpuArray(single([0 -0 -Inf -2])))), single([Inf Inf Inf Inf]));
assert_isequal(gather(acsch(gpuArray(single([0 -0])))), single([Inf -Inf]));
assert_isequal(gather(tanh(gpuArray(single([90 -90])))), single([1 -1]));
%=============================================================================
% real inputs outside the real domain give complex results
r = gather(sqrt(gpuArray(single([-4 9]))));
assert_isequal(r, complex(single([0 3]), single([2 0])));
r = gather(log(gpuArray(single([-1 1]))));
assert_isapprox(r, complex(single([0 0]), single([pi 0])), 1e-6);
r = gather(asin(gpuArray(single([2 -2]))));
assert_isapprox(double(r), [pi / 2 - 1.3169579i, -pi / 2 + 1.3169579i], 1e-5);
r = gather(acos(gpuArray(single([2 -2]))));
assert_isapprox(double(r), [1.3169579i, pi - 1.3169579i], 1e-5);
r = gather(acosh(gpuArray(single([0.5 -2 -Inf Inf]))));
assert_isapprox(double(r(1:2)), [1.0471976i, 1.3169579 + pi * 1i], 1e-5);
assert_isequal(r(3:4), complex(single([Inf Inf]), single([pi 0])));
r = gather(atanh(gpuArray(single([2 -2 Inf -Inf]))));
assert_isapprox(double(r(1:2)), [0.5493061 + pi / 2 * 1i, -0.5493061 - pi / 2 * 1i], 1e-5);
assert_isequal(r(3:4), complex(single([0 0]), single([pi / 2, -pi / 2])));
r = gather(asech(gpuArray(single([0 -0 2]))));
assert_isapprox(double(r(3)), 1.0471976i, 1e-5);
assert_isequal(r(1:2), complex(single([Inf Inf]), single([0 pi])));
r = gather(asind(gpuArray(single([Inf -Inf]))));
assert_isequal(r, complex(single([90 -90]), single([-Inf Inf])));
r = gather(sqrt(gpuArray(single([-1 NaN]))));
assert_isequal(isnan(real(r)), [false true]);
assert_isequal(imag(r(2)), single(0));
% power: whole exponents exact, negative base with a fractional exponent turns complex
g = gpuArray(single([-2 -1.5 0 1.5 3]));
assert_isequal(gather(g .^ 2), single([4 2.25 0 2.25 9]));
assert_isequal(gather(g .^ 3), single([-8 -3.375 0 3.375 27]));
h = single([-2 -1.5 0 1.5 3]);
assert_isapprox(gather(g .^ g), h .^ h, 1e-5);
assert_isapprox(gather(gpuArray(single(-8)) .^ (single(1) / 3)), single(1 + sqrt(3) * 1i), 1e-6);
%=============================================================================
% values the host rejects keep the host error
msg = _('Result must be real.');
assert_checkerror('realsqrt(gpuArray(single(-1)))', msg);
assert_checkerror('reallog(gpuArray(single(-1)))', msg);
%=============================================================================
% complex input still runs on the device for the core functions
c = gpuArray(single([1 + 2i, -3i]));
assert_isapprox(gather(sqrt(c)), sqrt(single([1 + 2i, -3i])), 1e-6);
assert_isapprox(gather(sin(c)), sin(single([1 + 2i, -3i])), 1e-5);
%=============================================================================
% regularized incomplete gamma, both tails (reference values)
x = single([0 0.001 0.5 1 2 10 30 80 Inf NaN]);
a = single([0.5 0.001 0.5 2 2 5 50 50 1 1]);
P = gammainc(gpuArray(x), gpuArray(a));
Q = gammainc(gpuArray(x), gpuArray(a), 'upper');
assert_istrue(isgpuarray(P) && isgpuarray(Q));
Pref = [0 0.993687647 0.682689492 0.264241118 0.593994150 0.970747312 0.000518892 0.999869216 1 NaN];
Qref = [1 0.006312353 0.317310508 0.735758882 0.406005850 0.029252688 0.999481109 0.000130784 0 NaN];
p = double(gather(P));
q = double(gather(Q));
assert_isequal(isnan(p), isnan(Pref));
assert_isapprox(p(~isnan(Pref)), Pref(~isnan(Pref)), 5e-5);
assert_isapprox(q(~isnan(Qref)), Qref(~isnan(Qref)), 5e-5);
assert_isapprox(double(gather(gammainc(gpuArray(single(1)), gpuArray(single(0.001)), 'upper'))), 0.0002196084, 1e-4);
% a negative argument keeps the host error
assert_checkerror('gammainc(gpuArray(single(-1)), gpuArray(single(2)))', _('X and A must contain nonnegative values.'));
%=============================================================================
% regularized incomplete beta, both tails (reference values)
x = single([0 0.0001 0.1 0.3 0.5 0.9 0.99 1]);
a = single([2 0.5 2 5 1 20 0.5 3]);
b = single([3 2 5 5 1 2 20 3]);
P = betainc(gpuArray(x), gpuArray(a), gpuArray(b));
Q = betainc(gpuArray(x), gpuArray(a), gpuArray(b), 'upper');
assert_istrue(isgpuarray(P) && isgpuarray(Q));
assert_isapprox(double(gather(P)), [0 0.0149994998 0.114265003 0.0988086746 0.5 0.364729829 1 1], 5e-5);
q = double(gather(Q));
assert_isapprox(q([1:6 8]), [1 0.9850005 0.885734997 0.901191325 0.5 0.635270171 0], 5e-5);
assert_istrue(q(7) < 1e-30);
assert_isapprox(double(gather(betainc(gpuArray(single([0.2 0.5 0.8])), 2, 3))), [0.180800005 0.6875 0.972800005], 5e-5);
% outside the interval keeps the host error
assert_checkerror('betainc(gpuArray(single(1.5)), 1, 1)', _('Wrong value for #1 argument. [0, 1] values expected.'));
%=============================================================================
