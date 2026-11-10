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
% more elements than one dispatch row can hold (65535 workgroups of 256)
n = 65535 * 256 + 1000;
G = gpuArray(ones(n, 1, 'single'));
R = gather(G + 1);
assert_isequal(nnz(R == 2), n);
assert_isequal(R(end), single(2));
%=============================================================================
% long lanes are reduced by many threads
assert_isequal(gather(sum(G)), single(n));
assert_isequal(gather(mean(G)), single(1));
assert_isequal(gather(prod(G)), single(1));
assert_istrue(gather(all(G)));
assert_istrue(gather(any(G)));
clear G R
%=============================================================================
x = rand(4e6, 1, 'single');
x(3000001) = 5;
X = gpuArray(x);
[m, i] = max(X);
assert_isequal(gather(m), single(5));
assert_isequal(gather(i), single(3000001));
[m, i] = min(X);
[refm, refi] = min(x);
assert_isequal(gather(m), refm);
assert_isequal(gather(i), single(refi));
assert_isequal(gather(max(X)), single(5));
assert_isapprox(double(gather(sum(X))), sum(double(x)), 1e-5);
assert_isapprox(double(gather(var(X))), var(double(x)), 1e-3);
assert_isapprox(double(gather(std(X))), std(double(x)), 1e-3);
%=============================================================================
% equal extremes: the first position wins
t = zeros(1, 5000, 'single');
t([10 4000]) = 7;
[~, i] = max(gpuArray(t));
assert_isequal(gather(i), single(10));
[~, i] = max(gpuArray(t'));
assert_isequal(gather(i), single(10));
%=============================================================================
% few long lanes and many short lanes give the same results
A = rand(3000, 4, 'single');
assert_isapprox(gather(sum(gpuArray(A))), sum(A), 1e-4);
assert_isapprox(gather(sum(gpuArray(A), 2)), sum(A, 2), 1e-5);
assert_isapprox(gather(sum(gpuArray(A'), 2)), sum(A', 2), 1e-4);
assert_isapprox(gather(mean(gpuArray(A'), 2)), mean(A', 2), 1e-5);
%=============================================================================
% complex and int32 long lanes
z = complex(ones(3e5, 1, 'single'), 2 * ones(3e5, 1, 'single'));
assert_isequal(gather(sum(gpuArray(z))), complex(single(3e5), single(6e5)));
z(123456) = complex(single(10), single(0));
assert_isequal(gather(max(gpuArray(z))), complex(single(10), single(0)));
k = int32(1:3e5)';
assert_isequal(gather(max(gpuArray(k))), int32(3e5));
assert_isequal(gather(min(gpuArray(k))), int32(1));
assert_isapprox(double(gather(sum(gpuArray(k)))), sum(double(k)), 1e-5);
%=============================================================================
% long scans
c = rand(3e6, 1, 'single');
C = gather(cumsum(gpuArray(c)));
ref = cumsum(double(c));
assert_istrue(max(abs(double(C) - ref)) / ref(end) < 1e-4);
assert_isequal(gather(cummax(gpuArray(c))), cummax(c));
assert_isequal(gather(cummin(gpuArray(c))), cummin(c));
zc = complex(ones(5e5, 1, 'single'), ones(5e5, 1, 'single'));
ZC = gather(cumsum(gpuArray(zc)));
assert_isequal(ZC(end), complex(single(5e5), single(5e5)));
%=============================================================================
% saturating integer scans keep the in-order result
s8 = int8(repmat([100; 100; -100], 1, 3));
assert_isequal(gather(cumsum(gpuArray(s8))), cumsum(s8));
long8 = int8(ones(2^20 + 10, 1));
long8(1:3) = int8([100; 100; -100]);
assert_isequal(gather(cumsum(gpuArray(long8))), cumsum(long8));
long32 = int32(ones(2^20 + 10, 1)) * int32(2^30);
assert_isequal(gather(cumsum(gpuArray(long32))), cumsum(long32));
%=============================================================================
% sort is stable and places NaN like the host
v = single([3 1 2 1 NaN 2 1]);
[s, i] = sort(gpuArray(v));
[refs, refi] = sort(v);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
[s, i] = sort(gpuArray(v), 'descend');
[refs, refi] = sort(v, 'descend');
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
M = single(randi(5, 7, 300));
[s, i] = sort(gpuArray(M), 2);
[refs, refi] = sort(M, 2);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
q = int32([5 -3 5 0 -3]);
[s, i] = sort(gpuArray(q));
[refs, refi] = sort(q);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
zs = single([1i, -1, 1, 2, -1i]);
[s, i] = sort(gpuArray(zs));
[refs, refi] = sort(zs);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
%=============================================================================
% long sort
w = rand(1e6, 1, 'single');
[s, i] = sort(gpuArray(w));
[refs, refi] = sort(w);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
%=============================================================================
% long inner dimension in a matrix product
y = rand(1e6, 1, 'single');
Y = gpuArray(y);
assert_isapprox(double(gather(Y' * Y)), sum(double(y) .^ 2), 1e-5);
B = rand(3, 2e4, 'single');
assert_isapprox(gather(gpuArray(B) * gpuArray(B')), B * B', 1e-4);
zy = complex(rand(1, 2e4, 'single'), rand(1, 2e4, 'single'));
ZY = gpuArray(zy);
assert_isapprox(gather(ZY * ZY.'), zy * zy.', 1e-4);
%=============================================================================
