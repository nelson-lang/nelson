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
asserts.isequal(nargin('pdist'), 3);
asserts.isequal(nargout('pdist'), 1);
%=============================================================================
X = [0 0; 1 0; 0 2];
D = pdist(X);
asserts.isapprox(D, [1 2 sqrt(5)], 1e-12);
%=============================================================================
asserts.isequal(pdist(X, 'cityblock'), [1 2 3]);
asserts.isequal(pdist(X, 'squaredeuclidean'), [1 4 5]);
asserts.isequal(pdist(X, 'sqeuclidean'), [1 4 5]);
asserts.isequal(pdist(X, 'chebychev'), [1 2 2]);
%=============================================================================
X = [1 0; 0 1; 1 1];
asserts.isapprox(pdist(X, 'cosine'), [1 1 - 1 / sqrt(2) 1 - 1 / sqrt(2)], 1e-12);
asserts.isapprox(pdist(X, 'hamming'), [1 0.5 0.5], 1e-12);
asserts.isapprox(pdist(X, 'jaccard'), [1 0.5 0.5], 1e-12);
%=============================================================================
X = [1 2 3; 2 4 6; 3 2 1];
asserts.isapprox(pdist(X, 'correlation'), [0 2 2], 1e-12);
asserts.isapprox(pdist(X, 'spearman'), [0 2 2], 1e-12);
X = [1 1 2 3; 1 2 2 4; 4 3 3 1];
asserts.isapprox(pdist(X, 'spearman'), [0.166666666666667 1.83333333333333 2], 1e-12);
%=============================================================================
X = [0 0; 1 1; 2 0];
asserts.isapprox(pdist(X, 'minkowski', 1.5), [1.587401051 2 1.587401051], 1e-9);
X = [-2 1; 0 -3; 4 5];
asserts.isapprox(pdist(X, 'minkowski', 3), [4.16016764610381 6.54213262037718 8.32033529220762], 1e-12);
%=============================================================================
X = [0 0; 2 0; 2 4];
asserts.isapprox(pdist(X, 'seuclidean'), [1.73205080756888 2.44948974278318 1.73205080756888], 1e-12);
asserts.isapprox(pdist(X, 'seuclidean', [2 4]), [1 1.41421356237310 1], 1e-12);
asserts.isapprox(pdist(X, 'mahalanobis', eye(2)), [2 sqrt(20) 4], 1e-12);
X = [0 1; 2 4; 3 8];
asserts.isapprox(pdist(X, 'seuclidean'), [1.56333471912302 2.79823441300328 1.31372323031479], 1e-12);
X = [1 2; 3 5; -1 4];
asserts.isapprox(pdist(X, 'mahalanobis', [2 0.4; 0.4 3]), [2.07727429415344 1.99313891631417 2.83084768755526], 1e-12);
%=============================================================================
asserts.isequal(pdist(zeros(1, 3)), zeros(1, 0));
asserts.isequal(pdist(zeros(0, 3)), zeros(1, 0));
asserts.isequal(pdist(zeros(3, 0)), [0 0 0]);
D = pdist(zeros(3, 0), 'hamming');
asserts.istrue(all(isnan(D)));
asserts.isequal(pdist(zeros(3, 0), 'jaccard'), [0 0 0]);
%=============================================================================
X = [NaN 0; 0 0; 1 1];
D = pdist(X, 'euclidean');
asserts.istrue(all(isnan(D(1:2))));
asserts.isapprox(D(3), sqrt(2), 1e-12);
D = pdist(X, 'chebychev');
asserts.istrue(all(isnan(D(1:2))));
asserts.isequal(D(3), 1);
D = pdist(X, 'hamming');
asserts.istrue(all(isnan(D(1:2))));
asserts.isequal(D(3), 1);
D = pdist(X, 'jaccard');
asserts.istrue(all(isnan(D(1:2))));
asserts.isequal(D(3), 1);
%=============================================================================
asserts.checkerror('pdist()', _('Not enough input arguments.'));
asserts.checkerror('pdist(ones(2, 2, 2))', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('pdist({1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('pdist([1], 1)', sprintf(_('%s must be a character vector or string scalar.'), 'Distance'));
asserts.checkerror('pdist([1], ''unknown'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('pdist([1], ''s'')', sprintf(_('Ambiguous %s value.'), 'Distance'));
asserts.checkerror('pdist([1 2; 3 4], ''minkowski'', 0)', sprintf(_('%s must be positive.'), 'Distance parameter'));
asserts.checkerror('pdist([1 2; 3 4], ''euclidean'', 2)', _('Distance parameter is not supported for this metric.'));
asserts.checkerror('pdist([1 2; 3 4], ''seuclidean'', [1 2 3])', sprintf(_('%s must be a vector.'), 'Distance parameter'));
asserts.checkerror('pdist([1 2; 3 4], ''mahalanobis'', eye(3))', sprintf(_('%s must have compatible size.'), 'Distance parameter'));
%=============================================================================
