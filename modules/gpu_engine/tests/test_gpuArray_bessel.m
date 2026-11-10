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
% real arguments run on the device in single precision
x = single([0.05 0.5 1 2.5 7 31 120]);
X = gpuArray(x);
xd = double(x);
M = sqrt(besselj(1.5, xd) .^ 2 + bessely(1.5, xd) .^ 2);
r = besselj(1.5, X);
assert_istrue(isgpuarray(r));
assert_isequal(classUnderlying(r), 'single');
assert_istrue(max(abs(double(gather(r)) - besselj(1.5, xd)) ./ M) < 1e-5);
assert_istrue(max(abs(double(gather(bessely(1.5, X))) - bessely(1.5, xd)) ./ M) < 1e-5);
assert_isapprox(double(gather(besseli(2, X, 1))), besseli(2, xd, 1), 1e-5);
assert_isapprox(double(gather(besselk(0.5, X))), besselk(0.5, xd), 1e-5);
assert_isapprox(double(gather(besselk(2.25, X, 1))), besselk(2.25, xd, 1), 1e-5);
% I(120) is beyond the single range: Inf on both sides
assert_isapprox(double(gather(besseli(-1.5, X))), double(single(besseli(-1.5, xd))), 1e-5);
%=============================================================================
% the order can be the gpuArray, same size arrays or a scalar
r = besselj(gpuArray(single([0 1 2])), single(3));
assert_istrue(isgpuarray(r));
assert_isapprox(double(gather(r)), besselj([0 1 2], 3), 1e-5);
r = besselj(gpuArray(single([0 1; 2 3])), gpuArray(single([1 2; 3 4])));
assert_isequal(size(r), [2 2]);
%=============================================================================
% Hankel functions give complex device results
r = besselh(0, 1, X);
assert_istrue(isgpuarray(r));
assert_isfalse(isreal(r));
H = besselh(0, 1, xd);
assert_istrue(max(abs(double(gather(r)) - H) ./ abs(H)) < 1e-5);
H = besselh(1, 2, xd, 1);
assert_istrue(max(abs(double(gather(besselh(1, 2, X, 1))) - H) ./ abs(H)) < 1e-5);
%=============================================================================
% integer orders stay real on the negative axis
xn = -x;
assert_isapprox(double(gather(besselj(3, gpuArray(xn)))), besselj(3, double(xn)), 1e-5, 1e-7);
assert_isapprox(double(gather(besseli(3, gpuArray(xn), 1))), besseli(3, double(xn), 1), 1e-5);
%=============================================================================
% special values
assert_isequal(gather(besselj(gpuArray(single([0 1 -0.5])), 0)), single([1 0 Inf]));
assert_isequal(gather(bessely(1, gpuArray(single(0)))), single(-Inf));
assert_isequal(gather(besselk(1, gpuArray(single([0 Inf])))), single([Inf 0]));
assert_istrue(isnan(gather(besselj(1, gpuArray(single(NaN))))));
%=============================================================================
% Airy functions
xa = single([-20 -3 -0.5 0 0.5 3 12]);
for k = 0:3
  ref = airy(k, double(xa));
  pair = mod(k, 2);
  scale = max(abs(ref), sqrt(airy(pair, double(xa)) .^ 2 + airy(pair + 2, double(xa)) .^ 2));
  assert_istrue(max(abs(double(gather(airy(k, gpuArray(xa)))) - ref) ./ scale) < 1e-5);
end
assert_isapprox(double(gather(airy(2, gpuArray(xa), 1))), airy(2, double(xa), 1), 1e-5);
%=============================================================================
% complex results are computed on the host
r = besselj(0.5, gpuArray(single([-1 2])));
assert_istrue(isgpuarray(r));
assert_isapprox(double(gather(r)), besselj(0.5, [-1 2]), 1e-6);
r = bessely(1, gpuArray(single(-2)));
assert_isfalse(isreal(r));
r = besselj(1, gpuArray(single(complex(1, 2))));
assert_isapprox(double(gather(r)), besselj(1, complex(1, 2)), 1e-6);
r = airy(0, gpuArray(single(-2)), 1);
assert_isfalse(isreal(r));
%=============================================================================
% the host reports the errors
assert_checkerror('besselj(gpuArray([1 2]), gpuArray([1 2 3]))', ...
  _('NU and Z must have the same size, or one of them must be a scalar.'));
assert_checkerror('besselh(1, 3, gpuArray(2))', _('K must be equal to 1 or 2.'));
%=============================================================================
