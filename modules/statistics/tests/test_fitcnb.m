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
asserts.isequal(nargin('fitcnb'), -3);
asserts.isequal(nargout('fitcnb'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 5 5; 5 6; 6 5; 10 0; 10 1; 11 0];
Y = [1; 1; 1; 2; 2; 2; 3; 3; 3];
mdl = fitcnb(X, Y);
asserts.isequal(class(mdl), 'ClassificationNaiveBayes');
asserts.isequal(mdl.NumObservations, 9);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.ClassNames, [1; 2; 3]);
asserts.isequal(mdl.Prior, [1 / 3 1 / 3 1 / 3]);
asserts.isapprox(mdl.Mu, [0.333333333333333 0.333333333333333; ...
  5.333333333333333 5.333333333333333; 10.333333333333334 0.333333333333333], 1e-12);
asserts.isapprox(mdl.Sigma, [0.471404520791032 0.471404520791032; ...
  0.471404520791032 0.471404520791032; 0.471404520791032 0.471404520791032], 1e-12);
[label, score, cost] = predict(mdl, [0.2 0.1; 5.2 5.1; 10.2 0.1]);
asserts.isequal(label, [1; 2; 3]);
asserts.isapprox(score, eye(3), 1e-12);
asserts.isapprox(cost, ones(3) - eye(3), 1e-12);
%=============================================================================
mdl = fitcnb(X, Y, 'Prior', [0.8 0.1 0.1]);
asserts.isequal(mdl.Prior, [0.8 0.1 0.1]);
[label, score] = predict(mdl, [3 3]);
asserts.isequal(label, 2);
asserts.isapprox(score, [0.004405183456244 0.995594816543756 0], 1e-12);
%=============================================================================
mdl = fitcnb(X, Y, 'Prior', 'uniform', 'ClassNames', [3; 2; 1]);
asserts.isequal(mdl.ClassNames, [3; 2; 1]);
asserts.isequal(mdl.Prior, [1 / 3 1 / 3 1 / 3]);
[label, score] = predict(mdl, [10.2 0.1; 0.2 0.1]);
asserts.isequal(label, [3; 1]);
asserts.isapprox(score, [1 0 0; 0 0 1], 1e-12);
%=============================================================================
species = {'setosa'; 'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'versicolor'};
X2 = [1 1; 1 2; 2 1; 5 5; 5 6; 6 5];
mdl = fitcnb(X2, species, 'ClassNames', {'versicolor'; 'setosa'});
[label, score] = predict(mdl, [5.2 5.1; 1.1 1.2]);
asserts.isequal(label, {'versicolor'; 'setosa'});
asserts.isapprox(score, [1 0; 0 1], 1e-12);
asserts.isequal(mdl.DistributionNames, {'normal' 'normal'});
asserts.isapprox(mdl.DistributionParameters{1, 1}, [5.333333333333333 0.471404520791032], 1e-12);
%=============================================================================
S = string(species);
mdl = fitcnb(X2, S, 'Prior', 'empirical');
label = predict(mdl, [1.4 1.3; 5.4 5.3]);
asserts.isequal(label, ["setosa"; "versicolor"]);
%=============================================================================
W = [3; 1; 1; 1; 1; 1; 1; 1; 1];
mdl = fitcnb(X, Y, 'Weights', W);
asserts.isapprox(mdl.Mu(1, :), [0.2 0.2], 1e-12);
%=============================================================================
asserts.checkerror('fitcnb()', _('Not enough input arguments.'));
asserts.checkerror('fitcnb({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcnb([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcnb([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcnb([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcnb([1; 2], [1; 2], ''Prior'', ''bad'')', sprintf(_('Invalid %s value.'), 'Prior'));
asserts.checkerror('fitcnb([1; 2], [1; 2], ''DistributionNames'', ''kernel'')', _('Only normal predictor distributions are supported.'));
asserts.checkerror('fitcnb([1; 2], [1; 2], ''Weights'', [1])', _('Weights must be a nonnegative finite numeric vector with positive sum.'));
asserts.checkerror('fitcnb([1; 2], [1; 2], ''ClassNames'', [1; 1])', _('ClassNames must contain unique labels.'));
asserts.checkerror('fitcnb([1; 2], [1; 2])', _('A normal distribution cannot be fit because one class has zero variance.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
