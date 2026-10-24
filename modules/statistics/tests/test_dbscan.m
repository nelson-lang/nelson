%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('dbscan'), -1);
asserts.isequal(nargout('dbscan'), -1);
%=============================================================================
X = [0 0; 0 0.1; 5 5; 5 5.1; 10 10];
[idx, corepts] = dbscan(X, 0.2, 2);
asserts.isequal(idx, [1; 1; 2; 2; -1]);
asserts.isequal(corepts, [true; true; true; true; false]);
%=============================================================================
x = [1 1.2 0.8 3.7 3.9 3.6 10; 1.1 0.8 1 4 3.9 4.1 10]';
idx = dbscan(x, 0.5, 2);
asserts.isequal(idx, [1; 1; 1; 2; 2; 2; -1]);
%=============================================================================
rng(0);
n = 10;
X1 = [rand(n, 2) + 1; rand(n, 2) + 5];
[idx, corepts] = dbscan(X1, 0.3, 4);
asserts.isequal(size(idx), [20 1]);
asserts.isequal(unique(idx), [-1; 1]);
asserts.isequal(find(corepts), [11; 16; 17; 19]);
%=============================================================================
X = [0 0; 0 1; 1 0; 5 5; 5 6; 6 5; 9 9; 9 10; 10 9];
idx = dbscan(X, 1, 2);
asserts.isequal(size(idx), [9 1]);
asserts.isequal(idx, [1; 1; 1; 2; 2; 2; 3; 3; 3]);
%=============================================================================
X = [0 0; 0 0.1; 5 5; 5 5.1; 10 10];
idx = dbscan(single(X), 0.2, 2);
asserts.isequal(idx, [1; 1; 2; 2; -1]);
asserts.isequal(class(idx), 'double');
%=============================================================================
idx = dbscan(zeros(0, 2), 1, 2);
asserts.isequal(size(idx), [0 1]);
%=============================================================================
D = abs([0; 0.1; 5; 5.1; 10] - [0 0.1 5 5.1 10]);
idx = dbscan(D, 0.2, 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1; 2; 2; -1]);
%=============================================================================
X = [0 0; 0.1 0.05; 5 5; 5.05 5.05];
idx = dbscan(X, 1, 2, 'Distance', 'squaredeuclidean');
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.2, 2, 'Distance', 'cityblock');
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.1, 2, 'Distance', 'chebychev');
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.2, 2, 'Distance', 'minkowski', 'P', 1);
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.2, 2, 'Distance', 'seuclidean', 'Scale', [1 1]);
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.2, 2, 'Distance', 'mahalanobis', 'Cov', eye(2));
asserts.isequal(idx, [1; 1; 2; 2]);
%=============================================================================
X = [0 0; 0.1 0; 0 0.1; 1 1; 1.1 1; 1 1.1; 5 5];
expected_labels = [1; 1; 1; 2; 2; 2; -1];
[idx, corepts] = dbscan(X, 0.25, 2, 'Distance', 'euclidean');
asserts.isequal(idx, expected_labels);
asserts.isequal(corepts, [true; true; true; true; true; true; false]);
idx = dbscan(X, 0.25, 2, 'Distance', 'squaredeuclidean');
asserts.isequal(idx, expected_labels);
idx = dbscan(X, 0.3, 2, 'Distance', 'cityblock');
asserts.isequal(idx, expected_labels);
idx = dbscan(X, 0.12, 2, 'Distance', 'chebychev');
asserts.isequal(idx, expected_labels);
idx = dbscan(X, 0.25, 2, 'Distance', 'minkowski', 'P', 3);
asserts.isequal(idx, expected_labels);
idx = dbscan(X, 0.3, 2, 'Distance', 'minkowski', 'P', 1);
asserts.isequal(idx, expected_labels);
[idx, corepts] = dbscan(X, 0.1, 2, 'Distance', 'cosine');
asserts.isequal(length(unique(idx)), 2);
asserts.isequal(find(corepts), [4; 5; 6; 7]);
[idx, corepts] = dbscan(X, 0.15, 2, 'Distance', 'correlation');
asserts.isequal(unique(idx(idx > 0)), [1; 2]);
asserts.isequal(find(corepts), [2; 3; 5; 6]);
%=============================================================================
X = [1 0; 2 0; 0 1; 0 2];
idx = dbscan(X, 0.01, 2, 'Distance', 'cosine');
asserts.isequal(idx, [1; 1; 2; 2]);
%=============================================================================
X = [1 2; 2 3; 10 9; 11 10];
idx = dbscan(X, 0.01, 2, 'Distance', 'correlation');
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.01, 2, 'Distance', 'spearman');
asserts.isequal(idx, [1; 1; 2; 2]);
%=============================================================================
X = [1 0 0; 1 0 0; 0 1 0; 0 1 0];
idx = dbscan(X, 0.01, 2, 'Distance', 'hamming');
asserts.isequal(idx, [1; 1; 2; 2]);
idx = dbscan(X, 0.01, 2, 'Distance', 'jaccard');
asserts.isequal(idx, [1; 1; 2; 2]);
%=============================================================================
X = [0 0; 0 0.1; 5 5; 5 5.1; 10 10];
f = @(row, A) sqrt((A(:, 1) - row(1)) .^ 2 + (A(:, 2) - row(2)) .^ 2);
idx = dbscan(X, 0.2, 2, 'Distance', f);
asserts.isequal(idx, [1; 1; 2; 2; -1]);
%=============================================================================
N = 90;
r1 = 0.5;
r2 = 5;
theta = linspace(0, 2 * pi, N)';
X1 = r1 * [cos(theta), sin(theta)];
X2 = r2 * [cos(theta), sin(theta)];
X = [X1; X2];
idx = dbscan(X, 1, 5);
asserts.isequal(unique(idx), [1; 2]);
idx = dbscan(X, 1, 5, 'Distance', 'squaredeuclidean');
asserts.isequal(unique(idx), [1; 2]);
%=============================================================================
X = [0; 0.1; 5; 5.1; 10];
D = abs(X - X');
Dv = [0.1 5 5.1 10 4.9 5 9.9 0.1 5 4.9];
[idx, corepts] = dbscan(D, 0.2, 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1; 2; 2; -1]);
asserts.isequal(corepts, [true; true; true; true; false]);
idx = dbscan(Dv, 0.2, 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1; 2; 2; -1]);
idx = dbscan(D <= 0.2, [], 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1; 2; 2; -1]);
idx = dbscan(Dv <= 0.2, [], 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1; 2; 2; -1]);
%=============================================================================
X = [0; 1; 2];
[idx, corepts] = dbscan(X, 1, 2);
asserts.isequal(idx, [1; 1; 1]);
asserts.isequal(corepts, [true; true; true]);
%=============================================================================
X = [0; 0.1];
[idx, corepts] = dbscan(X, 1, 3);
asserts.isequal(idx, [-1; -1]);
asserts.isequal(corepts, [false; false]);
%=============================================================================
X = [1 1; 1 1; 1 1; 5 5];
[idx, corepts] = dbscan(X, 1e-12, 3);
asserts.isequal(idx, [1; 1; 1; -1]);
asserts.isequal(corepts, [true; true; true; false]);
%=============================================================================
X = [0; 0.14; 0.28];
[idx, corepts] = dbscan(X, 0.15, 3);
asserts.isequal(idx, [1; 1; 1]);
asserts.isequal(corepts, [false; true; false]);
%=============================================================================
B = [false true; true false];
idx = dbscan(B, [], 2, 'Distance', 'precomputed');
asserts.isequal(idx, [1; 1]);
%=============================================================================
asserts.checkerror('dbscan()', _('Not enough input arguments.'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'')', _('Name-value arguments must be paired.'));
asserts.checkerror('dbscan([1 2], -1, 1)', sprintf(_('%s must be a positive scalar.'), 'Epsilon'));
asserts.checkerror('dbscan([1 2], 1, 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'MinPts'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''unknown'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''c'')', sprintf(_('Ambiguous %s value.'), 'Distance'));
asserts.checkerror('dbscan([1 2], 1, 1, ''P'', 3)', sprintf(_('%s is valid only with %s.'), 'P', 'Minkowski distance'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''minkowski'', ''P'', 0)', sprintf(_('%s must be a positive scalar.'), 'P'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Scale'', [1 1])', sprintf(_('%s is valid only with %s.'), 'Scale', 'standardized Euclidean distance'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''seuclidean'', ''Scale'', [1 1 1])', sprintf(_('%s must match the number of columns in X.'), 'Scale length'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''seuclidean'', ''Scale'', [1 0])', _('Scale values must be positive and finite.'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Cov'', eye(2))', sprintf(_('%s is valid only with %s.'), 'Cov', 'Mahalanobis distance'));
asserts.checkerror('dbscan([1 2], 1, 1, ''Distance'', ''mahalanobis'', ''Cov'', eye(3))', sprintf(_('%s must match the number of columns in X.'), 'Cov size'));
asserts.checkerror('dbscan([0 0; 1 1], 1, 1, ''Distance'', ''mahalanobis'', ''Cov'', [1 2; 2 1])', _('Cov must be symmetric positive definite.'));
asserts.checkerror('dbscan(D <= 0.2, 0.2, 2, ''Distance'', ''precomputed'')', _('Epsilon must be empty for logical precomputed distances.'));
asserts.checkerror('dbscan([1 2 3], [], 2, ''Distance'', ''precomputed'')', sprintf(_('%s must be a positive scalar.'), 'Epsilon'));
asserts.checkerror('dbscan([1 2 3 4], 1, 2, ''Distance'', ''precomputed'')', _('Precomputed distance vector has invalid length.'));
%=============================================================================
