%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
asserts.isequal(nargin('fitrknn'), -3);
asserts.isequal(nargout('fitrknn'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 5 5; 5 6; 6 5];
y = [1; 1.2; 1.1; 10; 10.2; 10.1];
mdl = fitrknn(X, y);
asserts.isequal(class(mdl), 'RegressionKNN');
asserts.isequal(mdl.NumNeighbors, 1);
asserts.isequal(mdl.NumObservations, 6);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isapprox(predict(mdl, [0.2 0.1; 5.2 5.1]), [1; 10], 1e-12);
%=============================================================================
mdl = fitrknn(X, y, 'NumNeighbors', 3);
[yfit, D] = predict(mdl, [2.5 2.5]);
asserts.isapprox(yfit, 1.1, 1e-12);
asserts.isapprox(D, [2.91547594742265 2.91547594742265 3.53553390593274], 1e-12);
%=============================================================================
mdl = fitrknn(X, y, 'NumNeighbors', 3, 'DistanceWeight', 'inverse');
asserts.isapprox(predict(mdl, [4.5 5.2]), 10.07772479694189, 1e-12);
mdl = fitrknn(X, y, 'NumNeighbors', 3, 'DistanceWeight', 'squaredinverse');
asserts.istrue(predict(mdl, [4.5 5.2]) > 10);
%=============================================================================
mdl = fitrknn(X, y, 'NumNeighbors', 3, 'Distance', 'minkowski', 'P', 1, 'Standardize', true);
asserts.istrue(mdl.Standardize);
asserts.isapprox(predict(mdl, [0.4 0.4; 5.4 5.4]), [1.1; 10.1], 1e-12);
%=============================================================================
mdl = fitrknn(X, y, 'Distance', 'seuclidean', 'Scale', [2 3]);
asserts.isapprox(predict(mdl, [0.1 0.2; 5.8 5.7]), [1; 10.1], 1e-12);
%=============================================================================
mdl = fitrknn(X, y, 'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y');
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
mdl = fitrknn(X, y, 'PredictorNames', ["A"; "B"]);
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
%=============================================================================
Xc = [1 2; 2 NaN; 3 4; 4 3; 5 6];
yc = [1; 2; NaN; 4; 5];
mc = fitrknn(Xc, yc, 'NumNeighbors', 2);
asserts.isequal(mc.NumObservations, 3);
asserts.isapprox(predict(mc, [1 2; 5 6]), [2.5; 4.5], 1e-12);
%=============================================================================
asserts.checkerror('fitrknn()', _('Not enough input arguments.'));
asserts.checkerror('fitrknn({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitrknn([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitrknn([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'response'));
asserts.checkerror('fitrknn(zeros(2,0), [1; 2])', _('X must contain at least one observation and one predictor.'));
asserts.checkerror('fitrknn([1; Inf], [1; 2])', _('X and Y must contain finite values or NaN missing values.'));
asserts.checkerror('fitrknn([NaN; NaN], [NaN; NaN])', _('No valid observations remain after removing missing values.'));
asserts.checkerror('fitrknn([1; 2], [1; 2], ''NumNeighbors'', 3)', _('NumNeighbors must not exceed the number of training observations.'));
asserts.checkerror('fitrknn([1; 2], [1; 2], ''NumNeighbors'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'NumNeighbors'));
asserts.checkerror('fitrknn([1; 2], [1; 2], ''Distance'', ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('fitrknn([1; 2], [1; 2], ''DistanceWeight'', ''bad'')', sprintf(_('Invalid %s value.'), 'DistanceWeight'));
asserts.checkerror('fitrknn([1; 2], [1; 2], ''Standardize'', 2)', sprintf(_('%s must be true or false.'), 'Standardize'));
asserts.checkerror('fitrknn([1 2; 3 4], [1; 2], ''Scale'', [1 0])', _('Scale must be a positive finite vector with one value per predictor.'));
asserts.checkerror('fitrknn(X, y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitrknn(X, y, ''ResponseName'', 1)', sprintf(_('%s must be a character vector or string scalar.'), 'ResponseName'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
