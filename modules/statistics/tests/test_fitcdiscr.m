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
asserts.isequal(nargin('fitcdiscr'), -3);
asserts.isequal(nargout('fitcdiscr'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6; 10 0; 10 1; 11 0; 11 1];
Y = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3];
mdl = fitcdiscr(X, Y);
asserts.isequal(class(mdl), 'ClassificationDiscriminant');
asserts.isequal(mdl.DiscrimType, 'linear');
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.ClassNames, [1; 2; 3]);
asserts.isequal(mdl.Prior, [1 / 3 1 / 3 1 / 3]);
asserts.isapprox(mdl.Mu, [0.5 0.5; 5.5 5.5; 10.5 0.5], 1e-12);
[label, score, cost] = predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 3]);
asserts.isapprox(score, eye(3), 1e-10);
asserts.isapprox(cost, ones(3) - eye(3), 1e-10);
%=============================================================================
mdl = fitcdiscr(X, Y, 'DiscrimType', 'quadratic');
asserts.isequal(mdl.DiscrimType, 'quadratic');
label = predict(mdl, [0.8 0.8; 5.6 5.4; 10.8 0.4]);
asserts.isequal(label, [1; 2; 3]);
%=============================================================================
mdl = fitcdiscr(X, Y, 'DiscrimType', 'diaglinear', 'Gamma', 0.2, 'Delta', 1e-4);
asserts.isequal(mdl.DiscrimType, 'diaglinear');
asserts.isequal(mdl.Gamma, 0.2);
asserts.isequal(mdl.Delta, 1e-4);
label = predict(mdl, [0.8 0.8; 5.6 5.4; 10.8 0.4]);
asserts.isequal(label, [1; 2; 3]);
mdl = fitcdiscr(X, Y, 'DiscrimType', 'diagquadratic');
label = predict(mdl, [0.8 0.8; 5.6 5.4; 10.8 0.4]);
asserts.isequal(label, [1; 2; 3]);
%=============================================================================
mdl = fitcdiscr(X, Y, 'Prior', [0.8 0.1 0.1]);
asserts.isequal(mdl.Prior, [0.8 0.1 0.1]);
[label, score] = predict(mdl, [2.5 2.5]);
asserts.isequal(label, 1);
asserts.isapprox(score, [0.999999961762211 0.000000038237789 0], 1e-12);
%=============================================================================
species = {'setosa'; 'setosa'; 'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'versicolor'; 'versicolor'};
X2 = [1 1; 1 2; 2 1; 2 2; 5 5; 5 6; 6 5; 6 6];
mdl = fitcdiscr(X2, species, 'ClassNames', {'versicolor'; 'setosa'});
[label, score] = predict(mdl, [5.2 5.1; 1.1 1.2]);
asserts.isequal(label, {'versicolor'; 'setosa'});
asserts.isapprox(score, [1 0; 0 1], 1e-10);
asserts.isequal(mdl.ClassNames, {'versicolor'; 'setosa'});
%=============================================================================
S = string(species);
mdl = fitcdiscr(X2, S);
label = predict(mdl, [1.4 1.3; 5.4 5.3]);
asserts.isequal(label, ["setosa"; "versicolor"]);
%=============================================================================
asserts.checkerror('fitcdiscr()', _('Not enough input arguments.'));
asserts.checkerror('fitcdiscr({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcdiscr([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcdiscr([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcdiscr([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcdiscr([1; 2], [1; 2])', _('Each class must contain at least two observations.'));
asserts.checkerror('fitcdiscr(X, Y, ''DiscrimType'', ''bad'')', sprintf(_('Invalid %s value.'), 'DiscrimType'));
asserts.checkerror('fitcdiscr(X, Y, ''Prior'', ''bad'')', sprintf(_('Invalid %s value.'), 'Prior'));
asserts.checkerror('fitcdiscr(X, Y, ''Gamma'', 2)', sprintf(_('%s must be a scalar in the range [0,1].'), 'Gamma'));
asserts.checkerror('fitcdiscr(X, Y, ''Delta'', -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Delta'));
asserts.checkerror('fitcdiscr(X, Y, ''ClassNames'', [1; 1; 2])', _('ClassNames must contain unique labels.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
