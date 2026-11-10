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
% polyval
r = polyval(gpuArray(single([1 2 3])), gpuArray(single([0 1 2])));
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), single([3 6 11]));
assert_isequal(gather(polyval([1 2 3], gpuArray(single([0; 1])))), single([3; 6]));
%=============================================================================
% conv: shape and orientation
r3 = gpuArray(single([1 2 3]));
c2 = gpuArray(single([4; 5]));
assert_isequal(size(conv(r3, c2)), [1 4]);
assert_isequal(size(conv(c2, r3)), [1 4]);
assert_isequal(size(conv(c2, c2)), [3 1]);
assert_isequal(size(conv(gpuArray(single(7)), c2)), [2 1]);
assert_isequal(gather(conv(r3, gpuArray(single([1 1])), 'same')), single([3 5 3]));
assert_isequal(gather(conv(r3, gpuArray(single([1 1 1 1])), 'same')), single([6 6 5]));
assert_isequal(gather(conv(gpuArray(single(1:5)), single([1 2 3]), 'valid')), single([10 16 22]));
assert_isequal(size(conv(r3, gpuArray(single(1:5)), 'valid')), [1 0]);
u = rand(1, 300, 'single');
v = rand(1, 40, 'single');
assert_isapprox(gather(conv(gpuArray(u), gpuArray(v))), conv(u, v), 1e-5);
%=============================================================================
% filter: recursive and moving-average filters along a dimension
assert_isapprox(gather(filter(1, [1 -0.5], gpuArray(single([1 1 1])))), single([1 1.5 1.75]), 1e-6);
assert_isapprox(gather(filter([1 1] / 2, 1, gpuArray(single([1 2 3 4])))), single([0.5 1.5 2.5 3.5]), 1e-6);
assert_isapprox(gather(filter([1 2], [2 1], gpuArray(single([1 0 0])))), single([0.5 0.75 -0.375]), 1e-6);
assert_isequal(size(filter(1, [1 -0.5], gpuArray(single([1; 1; 1])))), [3 1]);
X = rand(50, 4, 'single');
b = single([0.2 0.3 0.1]);
a = single([1 -0.4 0.2]);
assert_isapprox(gather(filter(b, a, gpuArray(X))), filter(b, a, X), 1e-5);
assert_isapprox(gather(filter(b, a, gpuArray(X), [], 2)), filter(b, a, X, [], 2), 1e-5);
%=============================================================================
% interp1
x = gpuArray(single([1 2 3]));
y = gpuArray(single([10 20 30]));
assert_isequal(gather(interp1(x, y, gpuArray(single([1.5 2.5])))), single([15 25]));
assert_isequal(gather(interp1(x, y, single([1.5 2.5]), 'nearest')), single([20 30]));
assert_istrue(all(isnan(gather(interp1(x, y, single([0 4]))))));
assert_isequal(gather(interp1(x, y, single([0 4]), 'linear', 'extrap')), single([0 40]));
assert_isequal(gather(interp1(x, y, single([0 4]), 'nearest', 'extrap')), single([10 30]));
r = gather(interp1(x, y, single([0.5 3 3.5]), 'previous'));
assert_isequal(isnan(r), [true false true]);
assert_isequal(r(2), single(30));
r = gather(interp1(x, y, single([0.5 1 3.5]), 'next'));
assert_isequal(r(2), single(10));
assert_isequal(gather(interp1(gpuArray(single([3 2 1])), gpuArray(single([30 20 10])), single(2.5))), single(25));
assert_isequal(gather(interp1(gpuArray(single([10 20 30])), single(2.5))), single(25));
r = gather(interp1(x, y, single([1.5 NaN]), 'linear', 0));
assert_isequal(r(1), single(15));
assert_istrue(isnan(r(2)));
assert_isequal(size(interp1(x, y, gpuArray(single([1.5; 2.5])))), [2 1]);
%=============================================================================
% trapz / cumtrapz
assert_isequal(gather(trapz(gpuArray(single([1 2 4])))), single(4.5));
assert_isequal(gather(trapz(2, gpuArray(single([1 2 4])))), single(9));
assert_isequal(gather(cumtrapz(gpuArray(single([1 2 4])))), single([0 1.5 4.5]));
assert_isequal(gather(trapz(gpuArray(single([1 2; 3 4; 5 6])))), single([6 8]));
assert_isequal(gather(trapz(gpuArray(single([1 2; 3 4])), 2)), single([1.5; 3.5]));
w = rand(5000, 1, 'single');
assert_isapprox(double(gather(trapz(gpuArray(w)))), trapz(double(w)), 1e-5);
assert_isapprox(double(gather(cumtrapz(gpuArray(w)))), cumtrapz(double(w)), 1e-4);
%=============================================================================
% 2-D convolution
rng(8);
A = single(rand(6, 7));
B = single(rand(3, 2));
for shape = {'full', 'same', 'valid'}
  G = conv2(gpuArray(A), gpuArray(B), shape{1});
  assert_istrue(isgpuarray(G));
  assert_isapprox(gather(G), conv2(A, B, shape{1}), 1e-5);
end
assert_isequal(size(conv2(gpuArray(single(ones(1, 6))), gpuArray(single(ones(3))), 'valid')), [0 4]);
u = single(rand(3, 1));
v = single(rand(1, 4));
assert_isapprox(gather(conv2(gpuArray(u), gpuArray(v), gpuArray(A), 'same')), conv2(u, v, A, 'same'), 1e-5);
%=============================================================================
% 2-D interpolation on grid vectors
rng(11);
V = single(rand(5, 7));
x = single([0 1 2 4 5 7 9]);
y = single([1 2 3 5 8]);
[XQ, YQ] = meshgrid(single(linspace(-1, 10, 13)), single(linspace(0, 9, 11)));
R = interp2(gpuArray(x), gpuArray(y), gpuArray(V), gpuArray(XQ), gpuArray(YQ));
assert_istrue(isgpuarray(R));
H = interp2(x, y, V, XQ, YQ);
assert_isequal(isnan(gather(R)), isnan(H));
assert_isapprox(gather(R(~isnan(H))), H(~isnan(H)), 1e-5);
assert_isapprox(gather(interp2(gpuArray(V), gpuArray(single([1.5 3.25 7])), gpuArray(single([2 4.5 5])))), interp2(V, single([1.5 3.25 7]), single([2 4.5 5])), 1e-5);
% nearest takes the upper point at a midpoint; a NaN query stays NaN with a fill value
W = single([1 2; 3 4]);
g = gpuArray(single([0 1]));
assert_isequal(gather(interp2(g, g, gpuArray(W), gpuArray(single([0.4 0.5 0.6])), gpuArray(single([0 0 0])), 'nearest')), single([1 2 2]));
assert_isequal(gather(interp2(g, g, gpuArray(W), gpuArray(single([0 0 0])), gpuArray(single([0.4 0.5 0.6])), 'nearest')), single([1 3 3]));
r = gather(interp2(g, g, gpuArray(W), gpuArray(single([NaN 2 0.5])), gpuArray(single([0.5 0.5 0.5])), 'linear', 0));
assert_istrue(isnan(r(1)));
assert_isequal(r(2:3), single([0 2.5]));
%=============================================================================
% 3-D and n-D interpolation on grid vectors
rng(12);
V3 = single(rand(4, 5, 3));
x = single([0 1 2 4 5]);
y = single([1 2 3 5]);
z = single([0 2 3]);
xq = single([-1 0.5 2.2 4.9 5]);
yq = single([1 1.5 4 5 2]);
zq = single([0 1 2.5 3 3.5]);
R = interp3(gpuArray(x), gpuArray(y), gpuArray(z), gpuArray(V3), gpuArray(xq), gpuArray(yq), gpuArray(zq));
assert_istrue(isgpuarray(R));
H = interp3(x, y, z, V3, xq, yq, zq);
assert_isequal(isnan(gather(R)), isnan(H));
assert_isapprox(gather(R(~isnan(H))), H(~isnan(H)), 1e-6);
F = interp3(gpuArray(x), gpuArray(y), gpuArray(z), gpuArray(V3), gpuArray(xq), gpuArray(yq), gpuArray(zq), 'linear', -7);
assert_isequal(gather(F([1 5])), single([-7 -7]));
N = interpn(gpuArray(y), gpuArray(x), gpuArray(z), gpuArray(V3), gpuArray(yq), gpuArray(xq), gpuArray(zq));
assert_isequal(isnan(gather(N)), isnan(H));
assert_isapprox(gather(N(~isnan(H))), H(~isnan(H)), 1e-6);
assert_isapprox(gather(interpn(gpuArray(V3), gpuArray(single([1.5 3.2])), gpuArray(single([2 4.5])), gpuArray(single([1 2.5])))), ...
  interpn(V3, single([1.5 3.2]), single([2 4.5]), single([1 2.5])), 1e-6);
% nearest takes the upper point at a midpoint (reference values)
W = reshape(single(1:60), 4, 5, 3);
Rn = interp3(gpuArray(x), gpuArray(y), gpuArray(z), gpuArray(W), gpuArray(xq), gpuArray(yq), gpuArray(zq), 'nearest');
assert_isequal(isnan(gather(Rn)), [true false false false true]);
assert_isequal(gather(Rn(2:4)), single([26 52 60]));
Rn2 = gather(interpn(gpuArray(y), gpuArray(x), gpuArray(W(:, :, 1)), gpuArray(yq), gpuArray(xq), 'nearest'));
assert_istrue(isnan(Rn2(1)));
assert_isequal(Rn2(2:5), single([6 12 20 18]));
%=============================================================================
% not-a-knot cubic spline values
rng(4);
x = single(sort(rand(1, 12)) * 10);
y = single(sin(x));
xq = single([-1 linspace(0, 10, 23) 11 NaN]);
g = spline(gpuArray(x), gpuArray(y), gpuArray(xq));
h = spline(x, y, xq);
assert_istrue(isgpuarray(g));
assert_isequal(isnan(gather(g)), isnan(h));
assert_isapprox(gather(g(~isnan(h))), h(~isnan(h)), 1e-4);
assert_isequal(class(h), 'single');
%=============================================================================
