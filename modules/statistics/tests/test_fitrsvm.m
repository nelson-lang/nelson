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
asserts.isequal(nargin('fitrsvm'), -3);
asserts.isequal(nargout('fitrsvm'), 1);
%=============================================================================
X = [0; 1; 2; 3; 4; 5];
y = [1; 1.2; 3; 3.2; 9; 9.1];
mdl = fitrsvm(X, y, 'KernelFunction', 'linear', 'BoxConstraint', 10);
asserts.isequal(class(mdl), 'RegressionSVM');
asserts.isequal(mdl.NumObservations, 6);
asserts.isequal(mdl.NumPredictors, 1);
asserts.isequal(mdl.KernelFunction, 'linear');
asserts.isequal(size(mdl.SupportVectors), [6 1]);
asserts.isapprox(mdl.Bias, 4.416666666666667, 1e-12);
asserts.isapprox(mdl.Alpha, [-34.16666666666667; -37.98336358136711; -25.80006049606768; ...
  -29.61675741076828; 22.56654567453106; 17.74984875983062], 1e-10);
asserts.isapprox(predict(mdl, [1.5; 4.5]), [5.289171203871784; 7.034180278281876], 1e-12);
%=============================================================================
mg = fitrsvm(X, y, 'KernelFunction', 'gaussian', 'KernelScale', 1, 'BoxConstraint', 10);
asserts.isequal(mg.KernelFunction, 'gaussian');
asserts.isapprox(predict(mg, [1.5; 4.5]), [2.282091150249824; 9.484562565411384], 1e-12);
mr = fitrsvm(X, y, 'KernelFunction', 'rbf', 'KernelScale', 1);
asserts.isequal(mr.KernelFunction, 'gaussian');
%=============================================================================
mp = fitrsvm(X, y, 'KernelFunction', 'polynomial', 'PolynomialOrder', 2, 'BoxConstraint', 5);
asserts.isequal(mp.PolynomialOrder, 2);
asserts.isapprox(predict(mp, [1.5; 4.5]), [2.143480758836726; 8.363671919397955], 1e-12);
%=============================================================================
Xs = [0 0; 0 1; 1 0; 4 4; 5 5; 6 5];
ys = [1; 1.2; 1.1; 8; 10; 11];
ms = fitrsvm(Xs, ys, 'Standardize', true, 'BoxConstraint', 2);
asserts.istrue(ms.Standardize);
asserts.isapprox(ms.Mu, [2.666666666666667 2.5], 1e-12);
asserts.isapprox(ms.Sigma, [2.658320271650251 2.428991560298224], 1e-12);
asserts.isapprox(predict(ms, [0.2 0.2; 5.2 5.1]), [1.075954752202989; 10.02967587092572], 1e-12);
%=============================================================================
mdl = fitrsvm(Xs, ys, 'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y', 'Epsilon', 0.25);
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
asserts.isequal(mdl.Epsilon, 0.25);
mdl = fitrsvm(Xs, ys, 'PredictorNames', ["A"; "B"]);
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
%=============================================================================
Xc = [1 2; 2 NaN; 3 4; 4 3; 5 6];
yc = [1; 2; NaN; 4; 5];
mc = fitrsvm(Xc, yc, 'BoxConstraint', 10);
asserts.isequal(mc.NumObservations, 3);
asserts.isapprox(predict(mc, [1 2; 5 6]), [3.010449241095235; 3.933846897007546], 1e-12);
%=============================================================================
asserts.checkerror('fitrsvm()', _('Not enough input arguments.'));
asserts.checkerror('fitrsvm({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitrsvm([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitrsvm([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'response'));
asserts.checkerror('fitrsvm(zeros(2,0), [1; 2])', _('X must contain at least one observation and one predictor.'));
asserts.checkerror('fitrsvm([1; Inf], [1; 2])', _('X and Y must contain finite values or NaN missing values.'));
asserts.checkerror('fitrsvm([NaN; NaN], [NaN; NaN])', _('No valid observations remain after removing missing values.'));
asserts.checkerror('fitrsvm(X, y, ''KernelFunction'', ''bad'')', sprintf(_('Invalid %s value.'), 'KernelFunction'));
asserts.checkerror('fitrsvm(X, y, ''KernelScale'', 0)', sprintf(_('%s must be a positive scalar.'), 'KernelScale'));
asserts.checkerror('fitrsvm(X, y, ''PolynomialOrder'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'PolynomialOrder'));
asserts.checkerror('fitrsvm(X, y, ''BoxConstraint'', 0)', sprintf(_('%s must be a positive scalar.'), 'BoxConstraint'));
asserts.checkerror('fitrsvm(X, y, ''Epsilon'', -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Epsilon'));
asserts.checkerror('fitrsvm(X, y, ''Standardize'', 2)', sprintf(_('%s must be true or false.'), 'Standardize'));
asserts.checkerror('fitrsvm(Xs, ys, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitrsvm(Xs, ys, ''ResponseName'', 1)', sprintf(_('%s must be a character vector or string scalar.'), 'ResponseName'));
asserts.checkerror('fitrsvm(X, y, ''K'', ''linear'')', _('Ambiguous name-value argument name.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
