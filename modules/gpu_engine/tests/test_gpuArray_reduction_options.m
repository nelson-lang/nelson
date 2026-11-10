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
A = single(reshape(1:24, 2, 3, 4));
G = gpuArray(A);
%=============================================================================
% 'all' and vectors of dimensions
assert_isequal(gather(sum(G, 'all')), sum(A, 'all'));
assert_isequal(gather(sum(G, [1 2])), sum(A, [1 2]));
assert_isequal(gather(sum(G, [2 3])), sum(A, [2 3]));
assert_isequal(gather(prod(gpuArray(single([1 2; 3 4])), 'all')), single(24));
assert_isequal(gather(mean(G, 'all')), mean(A, 'all'));
assert_isequal(gather(mean(G, [1 3])), mean(A, [1 3]));
assert_isequal(gather(max(G, [], 'all')), single(24));
assert_isequal(gather(min(G, [], [1 2])), min(A, [], [1 2]));
assert_isequal(gather(any(G > 23, 'all')), true);
assert_isequal(gather(all(G > 0, 'all')), true);
assert_isequal(gather(sum(G, 2)), sum(A, 2));
[m, i] = max(G, [], 'all');
assert_isequal(gather(m), single(24));
assert_isequal(gather(i), single(24));
assert_isapprox(gather(std(G, 0, 'all')), std(A(:)), 1e-5);
assert_isapprox(gather(var(G, 1, 'all')), var(A(:), 1), 1e-5);
%=============================================================================
% NaN: skipped by max/min/any by default, kept on request
v = single([NaN 1 2 NaN 5 3]);
g = gpuArray(v);
assert_isequal(gather(max(g)), single(5));
assert_isequal(gather(min(g)), single(1));
[m, i] = max(g);
assert_isequal([gather(m), gather(i)], single([5 5]));
[m, i] = min(g);
assert_isequal([gather(m), gather(i)], single([1 2]));
assert_istrue(isnan(gather(max(g, [], 'includenan'))));
[m, i] = max(g, [], 'includenan');
assert_istrue(isnan(gather(m)));
assert_isequal(gather(i), single(1));
assert_istrue(isnan(gather(max(gpuArray(single([NaN NaN]))))));
assert_isequal(gather(max(g, single(2))), single([2 2 2 2 5 3]));
assert_isequal(gather(min(g, single(2))), single([2 1 2 2 2 2]));
assert_isequal(gather(cummax(g)), cummax(v));
assert_isequal(gather(cummin(g)), cummin(v));
assert_isequal(gather(any(gpuArray(single(NaN)))), false);
assert_isequal(gather(all(gpuArray(single(NaN)))), true);
w = rand(1, 5000, 'single');
w(17) = NaN;
w(4000) = 3;
[m, i] = max(gpuArray(w));
assert_isequal([gather(m), gather(i)], single([3 4000]));
%=============================================================================
% 'omitnan' for sum, prod and mean
assert_isequal(gather(sum(g, 'omitnan')), single(11));
assert_isequal(gather(prod(g, 'omitnan')), single(30));
assert_isequal(gather(mean(g, 'omitnan')), single(11 / 4));
assert_istrue(isnan(gather(sum(g))));
assert_istrue(isnan(gather(mean(gpuArray(single([NaN NaN])), 'omitnan'))));
M = single([1 NaN; 3 4]);
assert_isequal(gather(mean(gpuArray(M), 'omitnan')), single([2 4]));
assert_isequal(gather(sum(gpuArray(M), 2, 'omitnan')), single([1; 7]));
%=============================================================================
% output class
I = gpuArray(int8([100 100 -10]));
assert_isequal(underlyingType(sum(I)), 'single');
% a native integer sum saturates at each step: 100 + 100 -> 127, then 127 - 10
assert_isequal(gather(sum(I, 'native')), int8(117));
assert_isequal(gather(sum(I, 'double')), single(190));
assert_isequal(gather(mean(I, 'native')), int8(63));
assert_isequal(gather(prod(gpuArray(single([])))), single(1));
assert_isequal(gather(sum(zeros(0, 3, 'gpuArray'), 'all')), single(0));
%=============================================================================
