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
asserts.isequal(nargin('fitcsvm'), -3);
asserts.isequal(nargout('fitcsvm'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6];
Y = [1; 1; 1; 1; 2; 2; 2; 2];
mdl = fitcsvm(X, Y, 'KernelFunction', 'linear', 'BoxConstraint', 1, 'IterationLimit', 5000);
asserts.isequal(class(mdl), 'ClassificationSVM');
asserts.isequal(mdl.NumObservations, 8);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.KernelFunction, 'linear');
asserts.isequal(mdl.ClassNames, [1; 2]);
asserts.isequal(mdl.ConvergenceInfo.Converged, true);
asserts.isequal(mdl.ConvergenceInfo.Iterations, 6);
asserts.isapprox(mdl.Bias, -1.5, 1e-12);
asserts.isapprox(mdl.Alpha, [0.0625; 0.0625], 1e-12);
asserts.isapprox(mdl.SupportVectors, [1 1; 5 5], 1e-12);
[label, score] = predict(mdl, [0.2 0.2; 5.2 5.2; 3 3]);
asserts.isequal(label, [1; 2; 2]);
asserts.isapprox(score, [1.4 -1.4; -1.1 1.1; 0 0], 1e-12);
%=============================================================================
mdl = fitcsvm(X, Y, 'KernelFunction', 'linear', 'Standardize', true, 'IterationLimit', 5000);
asserts.istrue(mdl.Standardize);
[label, score] = predict(mdl, [0.2 0.2; 5.2 5.2; 3 3]);
asserts.isequal(label, [1; 2; 2]);
asserts.isapprox(score, [1.14946341463415 -1.14946341463415; ...
  -1.08956097560976 1.08956097560976; -0.104390243902439 0.104390243902439], 1e-12);
%=============================================================================
Xg = [-1; -0.5; 0; 0.5; 1; 2; 2.5; 3; 3.5; 4];
Yg = [1; 1; 1; 1; 1; 2; 2; 2; 2; 2];
mg = fitcsvm(Xg, Yg, 'KernelFunction', 'gaussian', 'KernelScale', 1, ...
  'BoxConstraint', 2, 'IterationLimit', 5000);
[label, score] = predict(mg, [-0.25; 2.25]);
asserts.isequal(label, [1; 2]);
asserts.isequal(mg.KernelFunction, 'gaussian');
asserts.isapprox(score, [2.209743181260704 -2.209743181260704; ...
  -1.173581694050682 1.173581694050682], 1e-12);
%=============================================================================
mp = fitcsvm(Xg, Yg, 'KernelFunction', 'polynomial', 'PolynomialOrder', 2, ...
  'BoxConstraint', 1, 'IterationLimit', 5000);
[label, score] = predict(mp, [-0.25; 2.25]);
asserts.isequal(label, [1; 2]);
asserts.isequal(mp.PolynomialOrder, 2);
asserts.isapprox(score, [0.875 -0.875; -1.34722222222222 1.34722222222222], 1e-12);
%=============================================================================
species = {'setosa'; 'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'versicolor'};
xs = [1; 2; 3; 8; 9; 10];
ms = fitcsvm(xs, species, 'ClassNames', {'versicolor'; 'setosa'}, 'IterationLimit', 5000);
[label, score] = predict(ms, [2; 9]);
asserts.isequal(ms.ClassNames, {'versicolor'; 'setosa'});
asserts.isequal(label, {'setosa'; 'versicolor'});
asserts.isapprox(score, [-1.4 1.4; 1.4 -1.4], 1e-12);
S = string(species);
ms = fitcsvm(xs, S, 'IterationLimit', 5000);
asserts.isequal(predict(ms, [2; 9]), ["setosa"; "versicolor"]);
%=============================================================================
mdl = fitcsvm(X, Y, 'PredictorNames', {'A'; 'B'});
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
mdl = fitcsvm(X, Y, 'PredictorNames', ["A"; "B"]);
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
%=============================================================================
costMdl = fitcsvm(X, Y, 'KernelFunction', 'linear', 'Cost', [0 2; 3 0], ...
  'IterationLimit', 5000);
asserts.isequal(costMdl.Cost, [0 2; 3 0]);
asserts.isequal(costMdl.BoxConstraints(:), [2; 3]);
asserts.isequal(predict(costMdl, [0.2 0.2; 5.2 5.2]), [1; 2]);
%=============================================================================
asserts.checkerror('fitcsvm()', _('Not enough input arguments.'));
asserts.checkerror('fitcsvm({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcsvm([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcsvm([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcsvm([1; 2; 3], [1; 2; 3])', _('Y must contain exactly two classes.'));
asserts.checkerror('fitcsvm([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcsvm(X, Y, ''KernelFunction'', ''bad'')', sprintf(_('Invalid %s value.'), 'KernelFunction'));
asserts.checkerror('fitcsvm(X, Y, ''KernelScale'', 0)', sprintf(_('%s must be a positive scalar.'), 'KernelScale'));
asserts.checkerror('fitcsvm(X, Y, ''PolynomialOrder'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'PolynomialOrder'));
asserts.checkerror('fitcsvm(X, Y, ''BoxConstraint'', 0)', sprintf(_('%s must be a positive scalar.'), 'BoxConstraint'));
asserts.checkerror('fitcsvm(X, Y, ''Standardize'', 2)', sprintf(_('%s must be true or false.'), 'Standardize'));
asserts.checkerror('fitcsvm(X, Y, ''IterationLimit'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'IterationLimit'));
asserts.checkerror('fitcsvm(X, Y, ''Tolerance'', 0)', sprintf(_('%s must be a positive scalar.'), 'Tolerance'));
asserts.checkerror('fitcsvm(X, Y, ''ClassNames'', [1; 1])', _('ClassNames must contain unique labels.'));
asserts.checkerror('fitcsvm(X, Y, ''ClassNames'', [1; 2; 3])', _('ClassNames must include all observed classes.'));
asserts.checkerror('fitcsvm(X, Y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitcsvm(X, Y, ''Cost'', [0 -1; 1 0])', _('Cost must be a nonnegative square matrix with one row and column per class.'));
asserts.checkerror('fitcsvm(X, Y, ''K'', ''linear'')', _('Ambiguous name-value argument name.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
