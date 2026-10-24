%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('fitglm'), -3);
asserts.isequal(nargout('fitglm'), 1);
%=============================================================================
X = [1 2; 2 1; 3 4; 4 3; 5 6; 6 5];
y = 1 + 2 * X(:, 1) - 3 * X(:, 2);
mdl = fitglm(X, y);
asserts.isequal(class(mdl), 'GeneralizedLinearModel');
asserts.isequal(mdl.Distribution, 'normal');
asserts.isequal(mdl.Link, 'identity');
asserts.isequal(mdl.NumObservations, 6);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.DFE, 3);
asserts.isequal(mdl.Converged, true);
asserts.isapprox(mdl.Beta, [1; 2; -3], 1e-12);
asserts.isapprox(predict(mdl, [7 8; 8 7]), [-9; -4], 1e-12);
asserts.isapprox(mdl.Rsquared.Ordinary, 1, 1e-12);
asserts.istrue(mdl.RMSE < 1e-12);
asserts.isequal(mdl.Coefficients.Properties.VariableNames, {'Estimate', 'SE', 'tStat', 'pValue'});
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'; 'x1'; 'x2'});
%=============================================================================
mdl = fitglm(X, y, 'linear', 'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y');
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'; 'A'; 'B'});
%=============================================================================
mdl0 = fitglm(X, y, 'linear', 'Intercept', false);
asserts.isfalse(mdl0.Intercept);
asserts.isequal(mdl0.CoefficientNames, {'x1'; 'x2'});
asserts.isapprox(predict(mdl0, X), X * mdl0.Beta, 1e-12);
%=============================================================================
Xq = [1 2; 2 1; 3 4; 4 2; 5 6; 6 3; 7 8; 8 5];
yq = 1 + 2 * Xq(:, 1) + 3 * Xq(:, 2) + 4 * Xq(:, 1) .* Xq(:, 2) + 5 * Xq(:, 1) .^ 2;
mdlq = fitglm(Xq, yq, 'quadratic');
asserts.istrue(any(strcmp(mdlq.CoefficientNames, 'x1:x2')));
asserts.istrue(any(strcmp(mdlq.CoefficientNames, 'x1^2')));
asserts.isapprox(mdlq.Beta, [1; 2; 3; 4; 5; 0], 1e-10);
asserts.isapprox(predict(mdlq, Xq(1:3, :)), [22; 36; 112], 1e-10);
%=============================================================================
Xb = [-3; -2; -1; 0; 1; 2; 3; 4];
yb = [0; 1; 0; 0; 1; 0; 1; 1];
mb = fitglm(Xb, yb, 'Distribution', 'binomial');
asserts.isequal(mb.Link, 'logit');
asserts.isequal(mb.Converged, true);
asserts.isequal(mb.NumIterations, 5);
asserts.isapprox(mb.Beta, [-0.216600738330006; 0.433201476660013], 1e-12);
asserts.isapprox(predict(mb, [-1; 0; 1; 2]), ...
  [0.343034109110486; 0.446060535490379; 0.553939464509621; 0.656965890889514], 1e-12);
asserts.isapprox(mb.Deviance, 9.469238715559895, 1e-12);
asserts.isapprox(mb.Coefficients.SE, [0.808181993505254; 0.374865706552939], 1e-12);
asserts.isapprox(mb.Coefficients.pValue, [0.797667420164075; 0.291773093341903], 1e-12);
%=============================================================================
Xp = [0; 1; 2; 3; 4; 5; 6; 7];
yp = [1; 1; 2; 3; 5; 8; 13; 21];
mp = fitglm(Xp, yp, 'Distribution', 'poisson');
asserts.isequal(mp.Link, 'log');
asserts.isequal(mp.Converged, true);
asserts.isequal(mp.NumIterations, 4);
asserts.isapprox(mp.Beta, [-0.283374708694329; 0.474535276794544], 1e-12);
asserts.isapprox(predict(mp, [2; 4; 6]), [1.945844056009493; 5.026713538401194; 12.98554677036389], 1e-12);
asserts.isapprox(mp.Deviance, 0.120704939700564, 1e-12);
%=============================================================================
mp0 = fitglm([0; 1; 2; 3], [0; 1; 0; 2], 'Distribution', 'poisson');
asserts.istrue(mp0.Deviance > 0);
asserts.isapprox(predict(mp0, [0; 3]), [0.166200076742355; 1.692655414322268], 1e-12);
%=============================================================================
Xc = [1 2; 2 NaN; 3 4; 4 3; 5 6; 6 5];
yc = [1; 2; NaN; 4; 5; 6];
mc = fitglm(Xc, yc);
asserts.isequal(mc.NumObservations, 4);
asserts.isequal(size(mc.Fitted), [6 1]);
asserts.istrue(isnan(mc.Fitted(2)));
asserts.istrue(isnan(mc.Fitted(3)));
asserts.istrue(isnan(mc.Residuals.Raw(2)));
asserts.istrue(isnan(mc.Residuals.Raw(3)));
%=============================================================================
mdl = fitglm(X, y, 'constant');
asserts.isequal(mdl.CoefficientNames, {'(Intercept)'});
asserts.isapprox(predict(mdl, [10 20]), mean(y), 1e-12);
%=============================================================================
mn = fitglm(abs(X(:, 1)) + 1, abs(y) + 2, 'Distribution', 'normal', 'Link', 'log');
asserts.isequal(mn.Link, 'log');
asserts.isequal(size(predict(mn, [2; 3])), [2 1]);
%=============================================================================
asserts.checkerror('fitglm()', _('Not enough input arguments.'));
asserts.checkerror('fitglm({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitglm([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitglm([1; 2], [1])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('fitglm(zeros(2,0), [1; 2])', _('X must contain at least one predictor.'));
asserts.checkerror('fitglm([NaN; NaN], [NaN; NaN])', _('No valid observations remain after removing missing values.'));
asserts.checkerror('fitglm(X, y, ''bad'')', _('Name-value arguments must be paired.'));
asserts.checkerror('fitglm(X, y, [1 0 0])', sprintf(_('Model matrix must have one column per %s.'), 'predictor'));
asserts.checkerror('fitglm(X, y, ''Intercept'', 2)', sprintf(_('%s must be true or false.'), 'Intercept'));
asserts.checkerror('fitglm(X, y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitglm(X, y, ''Distribution'', ''gamma'')', sprintf(_('Invalid %s value.'), 'Distribution'));
asserts.checkerror('fitglm(X, y, ''Link'', ''probit'')', sprintf(_('Invalid %s value.'), 'Link'));
asserts.checkerror('fitglm(X, y, ''Distribution'', ''binomial'', ''Link'', ''identity'')', sprintf(_('Unsupported Link value for the %s distribution.'), 'binomial'));
asserts.checkerror('fitglm(Xb, [0; 1; 2; 0; 1; 0; 1; 1], ''Distribution'', ''binomial'')', _('Y must contain 0 or 1 values for the binomial distribution.'));
asserts.checkerror('fitglm(Xp, [1; 2; -1; 3; 4; 5; 6; 7], ''Distribution'', ''poisson'')', _('Y must contain nonnegative integer values for the poisson distribution.'));
asserts.checkerror('fitglm(X, y, ''MaxIter'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MaxIter'));
asserts.checkerror('fitglm(X, y, ''TolFun'', 0)', sprintf(_('%s must be a positive scalar.'), 'TolFun'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
