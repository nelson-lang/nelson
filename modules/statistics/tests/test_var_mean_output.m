%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% [V, M] = var(...): M is the mean used to compute the variance
A = [4 -7 3; 1 4 -2; 10 7 9];
[V, M] = var(A);
assert_isapprox(V, [21 54.3333 30.3333], 1e-4);
assert_isapprox(M, [5 4 / 3 10 / 3], 1e-12);
%=============================================================================
% normalization and dim
[V, M] = var(A, 1, 2);
assert_isapprox(V, [24.6667; 6; 1.5556], 1e-4);
assert_isapprox(M, [0; 1; 26 / 3], 1e-12);
%=============================================================================
% weights: M is the weighted mean
[V, M] = var(A, [1 2 3]);
assert_isapprox(V, [17 24.5556 24.5556], 1e-4);
assert_isapprox(M, [6 11 / 3 13 / 3], 1e-12);
[V, M] = var(A, [1 2 3], 2);
assert_isapprox(V, [23.4722; 7.25; 1.25], 1e-4);
assert_isapprox(M, [-1 / 6; 0.5; 8.5], 1e-12);
%=============================================================================
% 'all' and vecdim
[V, M] = var(A, 0, 'all');
assert_isapprox(V, 28.9444, 1e-4);
assert_isapprox(M, 29 / 9, 1e-12);
[V, M] = var(A, 0, [1 2]);
assert_isapprox(V, 28.9444, 1e-4);
assert_isapprox(M, 29 / 9, 1e-12);
X = reshape(1:24, [2 3 4]);
[V, M] = var(X, 0, [1 3]);
assert_isequal(size(V), [1 3]);
assert_isequal(M, [10.5 12.5 14.5]);
%=============================================================================
% nanflag
B = [1 NaN 3; 4 5 6];
[V, M] = var(B, 'omitnan');
assert_isequal(V, [4.5 0 4.5]);
assert_isequal(M, [2.5 5 4.5]);
[V, M] = var(B);
assert_isequal(V, [4.5 NaN 4.5]);
assert_isequal(M, [2.5 NaN 4.5]);
[V, M] = var(B, [1 2], 'omitnan');
assert_isequal(V, [2 0 2]);
assert_isequal(M, [3 5 5]);
[V, M] = var([1 NaN 3; 4 5 NaN], 0, [1 2], 'omitnan');
assert_isapprox(V, 2.9167, 1e-4);
assert_isequal(M, 3.25);
%=============================================================================
% classes, empty, scalar, complex, sparse
[V, M] = var(single([1 2 4]));
assert_isequal(class(V), 'single');
assert_isequal(class(M), 'single');
assert_isapprox(M, single(7 / 3), 1e-6);
[V, M] = var(single([1 2 4]), [1 1 2]);
assert_isequal(class(M), 'single');
assert_isequal(M, single(2.75));
[V, M] = var([]);
assert_isequal(V, NaN);
assert_isequal(M, NaN);
[V, M] = var(zeros(0, 3));
assert_isequal(V, NaN(1, 3));
assert_isequal(M, NaN(1, 3));
[V, M] = var(5);
assert_isequal(V, 0);
assert_isequal(M, 5);
[V, M] = var([1 + 2i 3 - 1i]);
assert_isequal(V, 6.5);
assert_isequal(M, 2 + 0.5i);
[V, M] = var(sparse([1 0 2]));
assert_istrue(issparse(V));
assert_istrue(issparse(M));
assert_isequal(full(V), 1);
assert_isequal(full(M), 1);
[V, M] = var(int8([1 2 4]));
assert_isequal(class(M), 'double');
assert_isequal(M, 7 / 3);
%=============================================================================
% std shares the same mean
[S, M] = std(B, [1 2], 'omitnan');
assert_isapprox(S, [sqrt(2) 0 sqrt(2)], 1e-12);
assert_isequal(M, [3 5 5]);
[S, M] = std(zeros(0, 3));
assert_isequal(M, NaN(1, 3));
[S, M] = std([1 NaN 3; 4 5 NaN], 0, [1 2], 'omitnan');
assert_isequal(M, 3.25);
%=============================================================================
