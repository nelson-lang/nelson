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
asserts.isequal(nargin('fitlm'), -3);
asserts.isequal(nargout('fitlm'), 1);
%=============================================================================
X = [1 2; 2 1; 3 4; 4 3; 5 6; 6 5];
y = 1 + 2 * X(:, 1) - 3 * X(:, 2);
mdl = fitlm(X, y);
asserts.isequal(class(mdl), 'LinearModel');
asserts.isequal(mdl.NumObservations, 6);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.DFE, 3);
asserts.isapprox(mdl.Beta, [1; 2; -3], 1e-12);
asserts.isapprox(predict(mdl, [7 8; 8 7]), [-9; -4], 1e-12);
asserts.isapprox(mdl.Rsquared.Ordinary, 1, 1e-12);
asserts.istrue(mdl.RMSE < 1e-12);
asserts.isequal(mdl.Coefficients.Properties.VariableNames, {'Estimate', 'SE', 'tStat', 'pValue'});
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'; 'x1'; 'x2'});
%=============================================================================
mdl = fitlm(X, y, 'linear', 'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y');
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'; 'A'; 'B'});
%=============================================================================
mdl0 = fitlm(X, y, 'linear', 'Intercept', false);
asserts.isfalse(mdl0.Intercept);
asserts.isequal(mdl0.CoefficientNames, {'x1'; 'x2'});
asserts.isapprox(predict(mdl0, X), X * mdl0.Beta, 1e-12);
%=============================================================================
Xq = [1 2; 2 1; 3 4; 4 2; 5 6; 6 3; 7 8; 8 5];
yq = 1 + 2 * Xq(:, 1) + 3 * Xq(:, 2) + 4 * Xq(:, 1) .* Xq(:, 2) + 5 * Xq(:, 1) .^ 2;
mdlq = fitlm(Xq, yq, 'quadratic');
asserts.istrue(any(strcmp(mdlq.CoefficientNames, 'x1:x2')));
asserts.istrue(any(strcmp(mdlq.CoefficientNames, 'x1^2')));
asserts.isapprox(predict(mdlq, Xq), yq, 1e-10);
%=============================================================================
yc = [1; 2; NaN; 4; 5; 6];
Xc = X;
Xc(2, 1) = NaN;
mdl = fitlm(Xc, yc);
asserts.isequal(mdl.NumObservations, 4);
asserts.isequal(size(mdl.Fitted), [6 1]);
asserts.istrue(isnan(mdl.Fitted(2)));
asserts.istrue(isnan(mdl.Residuals.Raw(3)));
%=============================================================================
mdl = fitlm(X, y, 'constant');
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'});
asserts.isapprox(predict(mdl, [10 20]), mean(y), 1e-12);
%=============================================================================
asserts.checkerror('fitlm()', _('Not enough input arguments.'));
asserts.checkerror('fitlm({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitlm([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitlm([1; 2], [1])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('fitlm(zeros(2,0), [1; 2])', _('X must contain at least one predictor.'));
asserts.checkerror('fitlm(X, y, ''bad'')', _('Name-value arguments must be paired.'));
asserts.checkerror('fitlm(X, y, [1 0 0])', sprintf(_('Model matrix must have one column per %s.'), 'predictor'));
asserts.checkerror('fitlm(X, y, ''Intercept'', 2)', sprintf(_('%s must be true or false.'), 'Intercept'));
asserts.checkerror('fitlm(X, y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
