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
asserts.isequal(nargin('fitrtree'), -3);
asserts.isequal(nargout('fitrtree'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6; 10 0; 10 1; 11 0; 11 1];
y = [1; 1.2; 1.1; 1.3; 10; 10.2; 10.1; 10.3; 20; 20.2; 20.1; 20.3];
mdl = fitrtree(X, y, 'MaxNumSplits', 4, 'MinLeafSize', 2, 'MinParentSize', 4);
asserts.isequal(class(mdl), 'RegressionTree');
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.MaxNumSplits, 4);
asserts.isequal(mdl.MinLeafSize, 2);
asserts.isequal(mdl.MinParentSize, 4);
asserts.isequal(sum(mdl.IsBranch), 4);
asserts.isequal(mdl.CutPredictor(1:4), [1; 1; 2; 2]);
asserts.isapprox(mdl.CutPoint(1:4), [8; 3; 0.5; 0.5], 1e-12);
asserts.isapprox(mdl.NodeMean, [10.48333333333333; 5.65; 20.15; 1.15; 10.15; 20.05; ...
  20.25; 1.05; 1.25], 1e-12);
[yfit, node] = predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isapprox(yfit, [1.05; 10.15; 20.05], 1e-12);
asserts.isequal(node, [8; 5; 6]);
asserts.isapprox(mdl.ResubstitutionLoss, 0.00583333333333337, 1e-14);
%=============================================================================
stump = fitrtree(X, y, 'MaxNumSplits', 1);
asserts.isequal(sum(stump.IsBranch), 1);
asserts.isapprox(stump.NodeMean, [10.48333333333333; 5.65; 20.15], 1e-12);
asserts.isapprox(predict(stump, [0.2 0.2; 5.2 5.2; 10.2 0.2]), ...
  [5.65; 5.65; 20.15], 1e-12);
%=============================================================================
mdl = fitrtree(X, y, 'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y');
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
mdl = fitrtree(X, y, 'PredictorNames', ["A"; "B"]);
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
%=============================================================================
Xc = [1 2; 2 NaN; 3 4; 4 3; 5 6];
yc = [1; 2; NaN; 4; 5];
mc = fitrtree(Xc, yc, 'MaxNumSplits', 2, 'MinParentSize', 2);
asserts.isequal(mc.NumObservations, 3);
asserts.isapprox(mc.NodeMean, [3.33333333333333; 1; 4.5; 4; 5], 1e-12);
asserts.isapprox(predict(mc, [1 2; 5 6]), [1; 5], 1e-12);
%=============================================================================
flat = fitrtree([1; 2; 3], [7; 7; 7]);
asserts.isequal(sum(flat.IsBranch), 0);
asserts.isapprox(predict(flat, [10; 20]), [7; 7], 1e-12);
%=============================================================================
asserts.checkerror('fitrtree()', _('Not enough input arguments.'));
asserts.checkerror('fitrtree({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitrtree([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitrtree([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'response'));
asserts.checkerror('fitrtree(zeros(2,0), [1; 2])', _('X must contain at least one observation and one predictor.'));
asserts.checkerror('fitrtree([1; Inf], [1; 2])', _('X and Y must contain finite values or NaN missing values.'));
asserts.checkerror('fitrtree([NaN; NaN], [NaN; NaN])', _('No valid observations remain after removing missing values.'));
asserts.checkerror('fitrtree(X, y, ''bad'')', _('Name-value arguments must be paired.'));
asserts.checkerror('fitrtree(X, y, ''M'', 1)', _('Ambiguous name-value argument name.'));
asserts.checkerror('fitrtree(X, y, ''MaxNumSplits'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'MaxNumSplits'));
asserts.checkerror('fitrtree(X, y, ''MinLeafSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinLeafSize'));
asserts.checkerror('fitrtree(X, y, ''MinParentSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinParentSize'));
asserts.checkerror('fitrtree(X, y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitrtree(X, y, ''ResponseName'', 1)', sprintf(_('%s must be a character vector or string scalar.'), 'ResponseName'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
