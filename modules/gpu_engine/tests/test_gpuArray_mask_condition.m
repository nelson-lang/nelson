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
% logical masks select positions, with the host result shapes
r = gpuArray(single([5 6 7]));
assert_isequal(gather(r(isfinite(r))), single([5 6 7]));
assert_isequal(gather(r(r > 5)), single([6 7]));
M = single(magic(3));
G = gpuArray(M);
assert_isequal(gather(G(G > 4)), M(M > 4));
assert_isequal(gather(G(logical([1 0 1]), :)), M(logical([1 0 1]), :));
assert_isequal(gather(G(:, [true false true])), M(:, [true false true]));
assert_isequal(size(G(G > 100)), size(M(M > 100)));
w = gpuArray(single([1 2 3]));
w(w > 1) = 0;
assert_isequal(gather(w), single([1 0 0]));
w(w > 5) = 9;
assert_isequal(gather(w), single([1 0 0]));
%=============================================================================
% conditions on device values
hit = false;
if gpuArray(true)
  hit = true;
end
assert_istrue(hit);
k = 0;
while gpuArray(k) < 3
  k = k + 1;
end
assert_isequal(k, 3);
assert_istrue(gpuArray(1) && gpuArray(2) > 1);
hit = false;
if gpuArray(single([1 0]))
  hit = true;
end
assert_isfalse(hit);
%=============================================================================
% size forms
A = gpuArray(ones(3, 4, 2));
assert_isequal(size(A, 'r'), 3);
assert_isequal(size(A, 'c'), 4);
assert_isequal(size(A, [1 3]), [3 2]);
assert_isequal(size(A, 2, 3), [4 2]);
assert_isequal(size(A, 5), 1);
[r1, c1] = size(A, [1 2]);
assert_isequal([r1 c1], [3 4]);
[r1, c1] = size(A);
assert_isequal([r1 c1], [3 8]);
%=============================================================================
% functions built from device operations stay on the device
rng(1);
X = single(rand(4, 3) + 0.1);
GX = gpuArray(X);
c = cov(GX);
assert_istrue(isgpuarray(c));
assert_isapprox(gather(c), cov(X), 1e-5);
assert_isapprox(gather(gradient(GX)), gradient(X), 1e-6);
assert_isapprox(gather(rescale(GX)), rescale(X), 1e-6);
assert_isapprox(gather(normalize(GX)), normalize(X), 1e-5);
p = single([0.1 1.2 2.9 3.4 6.5 7.0]);
assert_isapprox(gather(unwrap(gpuArray(p))), unwrap(p), 1e-5);
assert_isapprox(gather(poly(gpuArray(single([1 2 3])))), poly(single([1 2 3])), 1e-5);
assert_isequal(double(gather(rank(gpuArray(single(magic(4)))))), rank(single(magic(4))));
[t, rr] = cart2pol(gpuArray(single([1 0])), gpuArray(single([0 1])));
assert_istrue(isgpuarray(t) && isgpuarray(rr));
assert_isapprox(gather(t), single([0 pi / 2]), 1e-6);
%=============================================================================
% find with subscripts and values
H = single([0 2; 3 0]);
[i, j, v] = find(gpuArray(H));
[hi, hj, hv] = find(H);
assert_isequal(gather(i), single(hi));
assert_isequal(gather(j), single(hj));
assert_isequal(gather(v), hv);
assert_isequal(gather(find(gpuArray(H), 1)), single(find(H, 1)));
assert_isequal(gather(nonzeros(gpuArray(H))), nonzeros(H));
%=============================================================================
% device indices on host arrays
h = [10 20 30];
assert_isequal(h(gpuArray([1 3])), [10 30]);
assert_isequal(h(gpuArray(logical([1 0 1]))), [10 30]);
assert_isequal(size(h(gpuArray([1; 3]))), [1 2]);
h(gpuArray(2)) = 5;
assert_isequal(h, [10 5 30]);
M = magic(3);
assert_isequal(M(gpuArray(2), :), [3 5 7]);
assert_isequal(M(gpuArray(logical([1 0 1])), gpuArray(2)), [1; 9]);
assert_isequal(M(gpuArray(M > 5)), M(M > 5));
%=============================================================================
% device values assigned into host arrays: the result moves to the device
g = gpuArray(single([1 2 2 3]));
h = zeros(1, 3);
h(2) = g(1);
assert_istrue(isgpuarray(h));
assert_isequal(gather(h), single([0 1 0]));
h = zeros(2, 4);
h(1, :) = g;
assert_isequal(gather(h), single([1 2 2 3; 0 0 0 0]));
h = zeros(1, 4);
h(logical([1 0 1 0])) = g(2:3);
assert_isequal(gather(h), single([2 0 2 0]));
for k = 1:3
  q(k) = gpuArray(single(k));
end
assert_istrue(isgpuarray(q));
assert_isequal(gather(q), single([1 2 3]));
x = single([1 NaN 3 NaN NaN 6]);
assert_isequal(gather(fillmissing(gpuArray(x), 'linear')), fillmissing(x, 'linear'));
%=============================================================================
% device values as loop ranges, loop bounds and sizes
s = 0;
for k = gpuArray(single([3 1 4]))
  s = s + k;
end
assert_isequal(double(gather(s)), 8);
cols = 0;
for c = gpuArray(single([1 2; 3 4]))
  cols = cols + 1;
  assert_isequal(size(c), [2 1]);
end
assert_isequal(cols, 2);
s = 0;
for k = 1:gpuArray(single(3))
  s = s + k;
end
assert_isequal(s, 6);
assert_isequal(size(reshape(gpuArray(single(1:6)), 2, gpuArray(3))), [2 3]);
assert_isequal(size(reshape(gpuArray(single(1:6)), [], gpuArray(2))), [3 2]);
assert_isequal(size(reshape(1:6, 2, gpuArray(3))), [2 3]);
assert_isequal(gather(movmedian(gpuArray(single([3 1 4 1 5 9 2])), gpuArray(3))), movmedian(single([3 1 4 1 5 9 2]), 3));
%=============================================================================
% uniform cellfun/arrayfun outputs that are device values form a host array
g1 = gpuArray(single([1 2]));
g2 = gpuArray(single([3 4]));
r = cellfun(@(t) sum(t), {g1, g2});
assert_isequal(r, single([3 7]));
r = arrayfun(@(k) sum(gpuArray(single(k:3))), 1:3);
assert_isequal(r, single([6 5 3]));
c = cellfun(@(t) sum(t), {g1, g2}, 'UniformOutput', false);
assert_istrue(isgpuarray(c{1}));
assert_isequal(mat2str(cellfun(@(t) sum(t), {g1, g2})), '[3 7]');
%=============================================================================
% cells of device arrays are concatenated on the device
g1 = gpuArray(single([1 2]));
g2 = gpuArray(single([3 4]));
r = cell2mat({g1, g2; g2, g1});
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), single([1 2 3 4; 3 4 1 2]));
assert_isequal(gather(cell2mat({g1; g2})), single([1 2; 3 4]));
%=============================================================================
