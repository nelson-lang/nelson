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
asserts.isequal(nargin('fitcecoc'), -3);
asserts.isequal(nargout('fitcecoc'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6; 10 0; 10 1; 11 0; 11 1];
Y = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3];
mdl = fitcecoc(X, Y, 'KernelFunction', 'linear', 'Standardize', true, 'IterationLimit', 5000);
asserts.isequal(class(mdl), 'ClassificationECOC');
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.ClassNames, [1; 2; 3]);
asserts.isequal(length(mdl.BinaryLearners), 3);
asserts.isequal(mdl.CodingMatrix, [-1 -1 0; 1 0 -1; 0 1 1]);
[label, score] = predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 3]);
asserts.isapprox(score, [2 / 3 1 / 3 0; 0 2 / 3 1 / 3; 0 1 / 3 2 / 3], 1e-12);
%=============================================================================
mg = fitcecoc(X, Y, 'KernelFunction', 'gaussian', 'KernelScale', 2, 'BoxConstraint', 2);
label = predict(mg, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 3]);
mp = fitcecoc(X, Y, 'KernelFunction', 'polynomial', 'PolynomialOrder', 2, 'IterationLimit', 5000);
asserts.isequal(mp.LearnerOptions.KernelFunction, 'polynomial');
%=============================================================================
species = {'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'virginica'; 'virginica'};
X2 = [1 1; 1 2; 4 4; 4 5; 8 8; 8 9];
mt = fitcecoc(X2, species, 'ClassNames', {'virginica'; 'versicolor'; 'setosa'}, ...
  'IterationLimit', 5000);
[label, score] = predict(mt, [7.9 8.1; 1.1 1.2; 4.2 4.4]);
asserts.isequal(mt.ClassNames, {'virginica'; 'versicolor'; 'setosa'});
asserts.isequal(label, {'virginica'; 'setosa'; 'versicolor'});
asserts.isapprox(score, [2 / 3 1 / 3 0; 0 1 / 3 2 / 3; 0 2 / 3 1 / 3], 1e-12);
S = string(species);
ms = fitcecoc(X2, S, 'IterationLimit', 5000);
asserts.isequal(predict(ms, [1.1 1.2; 4.2 4.4; 7.9 8.1]), ...
  ["setosa"; "versicolor"; "virginica"]);
%=============================================================================
asserts.checkerror('fitcecoc()', _('Not enough input arguments.'));
asserts.checkerror('fitcecoc({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcecoc([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcecoc([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcecoc([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcecoc(X, Y, ''KernelFunction'', ''bad'')', sprintf(_('Invalid %s value.'), 'KernelFunction'));
asserts.checkerror('fitcecoc(X, Y, ''KernelScale'', 0)', sprintf(_('%s must be a positive scalar.'), 'KernelScale'));
asserts.checkerror('fitcecoc(X, Y, ''PolynomialOrder'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'PolynomialOrder'));
asserts.checkerror('fitcecoc(X, Y, ''BoxConstraint'', 0)', sprintf(_('%s must be a positive scalar.'), 'BoxConstraint'));
asserts.checkerror('fitcecoc(X, Y, ''Standardize'', 2)', sprintf(_('%s must be true or false.'), 'Standardize'));
asserts.checkerror('fitcecoc(X, Y, ''IterationLimit'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'IterationLimit'));
asserts.checkerror('fitcecoc(X, Y, ''Tolerance'', 0)', sprintf(_('%s must be a positive scalar.'), 'Tolerance'));
asserts.checkerror('fitcecoc(X, Y, ''ClassNames'', [1; 1; 2])', _('ClassNames must contain unique labels.'));
asserts.checkerror('fitcecoc(X, Y, ''ClassNames'', [1; 2])', _('ClassNames must include all observed classes.'));
asserts.checkerror('fitcecoc(X, Y, ''K'', ''linear'')', _('Ambiguous name-value argument name.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
