%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('pdist2'), -1);
asserts.isequal(nargout('pdist2'), 2);
%=============================================================================
X = [0 0; 1 0];
Y = [0 0; 0 2];
D = pdist2(X, Y);
asserts.isapprox(D, [0 2; 1 sqrt(5)], 1e-12);
[D, I] = pdist2(X, Y, 'euclidean', 'Smallest', 1);
asserts.isapprox(D, [0 2], 1e-12);
asserts.isequal(I, [1 1]);
[D, I] = pdist2(X, Y, 'euclidean', 'Largest', 1);
asserts.isapprox(D, [1 sqrt(5)], 1e-12);
asserts.isequal(I, [2 2]);
%=============================================================================
asserts.isequal(pdist2(X, Y, 'cityblock'), [0 2; 1 3]);
asserts.isequal(pdist2(X, Y, 'chebychev'), [0 2; 1 2]);
asserts.isequal(pdist2(X, Y, 'chebyshev'), [0 2; 1 2]);
asserts.isequal(pdist2(X, Y, 'squaredeuclidean'), [0 4; 1 5]);
asserts.isequal(pdist2(X, Y, 'sqeuclidean'), [0 4; 1 5]);
asserts.isapprox(pdist2(X, Y, 'fasteuclidean'), [0 2; 1 sqrt(5)], 1e-12);
%=============================================================================
asserts.isequal(pdist2([0 0; 2 0], Y, 'cityblock'), [0 2; 2 4]);
%=============================================================================
X = [1 0; 0 1];
Y = [1 0; 1 1];
asserts.isapprox(pdist2(X, Y, 'cosine'), [0 1 - 1 / sqrt(2); 1 1 - 1 / sqrt(2)], 1e-12);
asserts.isapprox(pdist2(X, Y, 'hamming'), [0 0.5; 1 0.5], 1e-12);
asserts.isapprox(pdist2(X, Y, 'jaccard'), [0 0.5; 1 0.5], 1e-12);
%=============================================================================
X = [1 2 3; 1 3 5];
Y = [2 4 6; 3 2 1];
D = pdist2(X, Y, 'correlation');
asserts.isequal(size(D), [2 2]);
asserts.isapprox(D, [0 2; 0 2], 1e-10);
D = pdist2(X, Y, 'spearman');
asserts.isapprox(D, [0 2; 0 2], 1e-10);
%=============================================================================
X = [0 0; 2 0; 0 2];
Y = [1 1; 2 2];
asserts.isapprox(pdist2(X, Y, 'minkowski', 1), [2 4; 2 2; 2 2], 1e-12);
asserts.isapprox(pdist2(X, Y, 'seuclidean', [1 2]), [sqrt(1.25) sqrt(5); sqrt(1.25) 1; sqrt(1.25) 2], 1e-12);
asserts.isapprox(pdist2(X, Y, 'fastseuclidean', [1 2]), [sqrt(1.25) sqrt(5); sqrt(1.25) 1; sqrt(1.25) 2], 1e-12);
asserts.isapprox(pdist2(X, Y, 'mahalanobis', eye(2)), [sqrt(2) sqrt(8); sqrt(2) 2; sqrt(2) 2], 1e-12);
[D, I] = pdist2(X, Y, 'sqeuclidean', 'Smallest', 2);
asserts.isequal(D, [2 4; 2 4]);
asserts.isequal(I, [1 2; 2 3]);
[D, I] = pdist2(X, Y, 'sqeuclidean', 'Largest', 2);
asserts.isequal(D, [2 8; 2 4]);
asserts.isequal(I, [1 1; 2 2]);
%=============================================================================
f = @(row, A) abs(A(:, 1) - row(1)) + abs(A(:, 2) - row(2));
asserts.isequal(pdist2([0 0; 2 0], [1 1; 2 2], f), [2 4; 2 2]);
%=============================================================================
D = pdist2([NaN 0; 0 0], [0 0; 1 1], 'hamming');
asserts.istrue(all(isnan(D(1, :))));
asserts.isapprox(D(2, :), [0 1], 1e-12);
%=============================================================================
asserts.checkerror('pdist2()', _('Not enough input arguments.'));
asserts.checkerror('pdist2([1 2], [1; 2])', sprintf(_('X and Y must have %s.'), 'the same number of columns'));
asserts.checkerror('pdist2({1}, [1])', _('X and Y must be numeric matrices.'));
asserts.checkerror('pdist2([1], [1], 1)', sprintf(_('%s must be a character vector or string scalar.'), 'Distance'));
asserts.checkerror('pdist2([1], [1], ''unknown'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('pdist2([1], [1], ''s'')', sprintf(_('Ambiguous %s value.'), 'Distance'));
asserts.checkerror('[D, I] = pdist2([1], [1])', _('Smallest or Largest must be specified to return indices.'));
asserts.checkerror('pdist2([1], [1], ''euclidean'', 2)', _('Distance parameter is not supported for this metric.'));
asserts.checkerror('pdist2([1], [1], ''minkowski'', 0)', sprintf(_('%s must be positive.'), 'Distance parameter'));
asserts.checkerror('pdist2([1], [1], ''Smallest'')', _('Name-value arguments must be paired.'));
asserts.checkerror('pdist2([1], [1], ''Smallest'', 1, ''Largest'', 1)', _('Smallest and Largest cannot both be specified.'));
%=============================================================================
