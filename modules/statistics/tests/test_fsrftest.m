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
asserts.isequal(nargin('fsrftest'), -3);
asserts.isequal(nargout('fsrftest'), 2);
%=============================================================================
X = [1 0 3; 2 1 2; 3 0 1; 4 1 2; 5 0 3; 6 1 4];
Y = [1; 2; 2; 4; 4; 7];
[idx, scores] = fsrftest(X, Y, 'NumBins', 2);
asserts.isequal(idx, [1 3 2]);
asserts.isapprox(scores, [30.507042253521110 1.384615384615384 2.470588235294117], 1e-12);
%=============================================================================
[idx, scores] = fsrftest(single(X), single(Y), 'NumBins', 2);
asserts.isequal(idx, [1 3 2]);
asserts.isequal(class(scores), 'double');
asserts.isapprox(scores, [30.507042253521110 1.384615384615384 2.470588235294117], 1e-12);
%=============================================================================
X2 = [1 1; 1 2; 2 1; 2 2; 3 1; 3 2];
Y2 = [1; 2; 2; 3; 4; 5];
[idx, scores] = fsrftest(X2, Y2, 'CategoricalPredictors', 1, 'NumBins', 2);
asserts.isequal(idx, [1 2]);
asserts.isapprox(scores, [9.333333333333332 0.642857142857143], 1e-12);
%=============================================================================
X3 = [1 NaN; 2 1; NaN 1; 4 2; 5 2; 6 NaN];
Y3 = [1; 2; 3; 4; 6; 5];
[idx, scores] = fsrftest(X3, Y3, 'UseMissing', true, ...
  'CategoricalPredictors', [2], 'Weights', [1; 2; 1; 1; 2; 1], 'NumBins', 2);
asserts.isequal(idx, [1 2]);
asserts.isapprox(scores, [23.779280765423994 1.924632352941176], 1e-12);
%=============================================================================
X4 = [(1:12)' [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6]];
Y4 = [1; 1; 2; 2; 2; 3; 4; 4; 5; 5; 5; 6];
[idx, scores] = fsrftest(X4, Y4);
asserts.isequal(idx, [2 1]);
asserts.isapprox(scores, [7.037037037037038 200.429447852760319], 1e-12);
%=============================================================================
[idx, scores] = fsrftest(X4, Y4, 'CategoricalPredictors', 'all');
asserts.isequal(size(idx), [1 2]);
asserts.isequal(size(scores), [1 2]);
[idx, scores] = fsrftest(X4, Y4, 'CategoricalPredictors', [false true]);
asserts.isequal(size(idx), [1 2]);
asserts.isequal(size(scores), [1 2]);
%=============================================================================
asserts.checkerror('fsrftest()', _('Not enough input arguments.'));
asserts.checkerror('fsrftest({1}, 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fsrftest([1; 2], {1; 2})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fsrftest([1; 2], [1; 2; 3])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('fsrftest(zeros(2, 0), [1; 2])', _('X must contain at least one predictor.'));
asserts.checkerror('fsrftest([Inf; 2], [1; 2])', sprintf(_('%s must not contain Inf values.'), 'X and Y'));
asserts.checkerror('fsrftest(X, Y, ''CategoricalPredictors'', [4])', sprintf(_('%s contains invalid predictor indices.'), 'CategoricalPredictors'));
asserts.checkerror('fsrftest(X, Y, ''CategoricalPredictors'', [true false])', sprintf(_('%s must match the number of predictors.'), 'CategoricalPredictors'));
asserts.checkerror('fsrftest(X, Y, ''CategoricalPredictors'', ''bad'')', sprintf(_('%s has an invalid value.'), 'CategoricalPredictors'));
asserts.checkerror('fsrftest(X, Y, ''NumBins'', 1)', _('NumBins must be an integer scalar greater than one.'));
asserts.checkerror('fsrftest(X, Y, ''UseMissing'', 2)', sprintf(_('%s must be a logical scalar.'), 'UseMissing'));
asserts.checkerror('fsrftest(X, Y, ''Weights'', [1; 2])', _('Weights must be a real numeric vector with one value per observation.'));
asserts.checkerror('fsrftest(X, Y, ''Weights'', [1; 1; 1; 1; 1; -1])', sprintf(_('%s must contain nonnegative finite values with positive sum.'), 'Weights'));
asserts.checkerror('fsrftest(X, Y, ''Unknown'', 1)', _('Unknown option.'));
%=============================================================================
