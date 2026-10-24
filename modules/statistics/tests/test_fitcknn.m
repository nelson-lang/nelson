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
asserts.isequal(nargin('fitcknn'), -3);
asserts.isequal(nargout('fitcknn'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 5 5; 5 6; 6 5];
Y = [1; 1; 1; 2; 2; 2];
mdl = fitcknn(X, Y);
asserts.isequal(class(mdl), 'ClassificationKNN');
asserts.isequal(mdl.NumNeighbors, 1);
asserts.isequal(mdl.NumObservations, 6);
asserts.isequal(mdl.NumPredictors, 2);
[label, score, cost] = predict(mdl, [0.2 0.1; 5.2 5.1]);
asserts.isequal(label, [1; 2]);
asserts.isequal(score, [1 0; 0 1]);
asserts.isequal(cost, [0 1; 1 0]);
%=============================================================================
mdl = fitcknn(X, Y, 'NumNeighbors', 5);
[label, score] = predict(mdl, [2.5 2.5]);
asserts.isequal(label, 1);
asserts.isequal(score, [0.6 0.4]);
%=============================================================================
mdl = fitcknn(X, Y, 'NumNeighbors', 3, 'DistanceWeight', 'inverse');
[label, score] = predict(mdl, [4.5 5.2]);
asserts.isequal(label, 2);
asserts.isequal(score, [0 1]);
%=============================================================================
mdl = fitcknn(X, Y, 'NumNeighbors', 3, 'Distance', 'minkowski', 'P', 1, 'Standardize', true);
label = predict(mdl, [0.4 0.4; 5.4 5.4]);
asserts.isequal(label, [1; 2]);
asserts.istrue(mdl.Standardize);
%=============================================================================
mdl = fitcknn(X, Y, 'Distance', 'seuclidean', 'Scale', [2 3]);
label = predict(mdl, [0.1 0.2; 5.8 5.7]);
asserts.isequal(label, [1; 2]);
%=============================================================================
species = {'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'virginica'; 'virginica'};
X2 = [1 1; 1 2; 4 4; 4 5; 8 8; 8 9];
mdl = fitcknn(X2, species, 'NumNeighbors', 1, 'ClassNames', {'virginica'; 'versicolor'; 'setosa'});
[label, score] = predict(mdl, [7.9 8.1; 1.1 1.2]);
asserts.isequal(label, {'virginica'; 'setosa'});
asserts.isequal(score, [1 0 0; 0 0 1]);
asserts.isequal(mdl.ClassNames, {'virginica'; 'versicolor'; 'setosa'});
%=============================================================================
S = string(species);
mdl = fitcknn(X2, S, 'NumNeighbors', 1);
label = predict(mdl, [4.1 4.2]);
asserts.isequal(label, "versicolor");
%=============================================================================
asserts.checkerror('fitcknn()', _('Not enough input arguments.'));
asserts.checkerror('fitcknn({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcknn([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcknn([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcknn([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcknn([1; 2], [1; 2], ''NumNeighbors'', 3)', _('NumNeighbors must not exceed the number of training observations.'));
asserts.checkerror('fitcknn([1; 2], [1; 2], ''NumNeighbors'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'NumNeighbors'));
asserts.checkerror('fitcknn([1; 2], [1; 2], ''Distance'', ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('fitcknn([1; 2], [1; 2], ''DistanceWeight'', ''bad'')', sprintf(_('Invalid %s value.'), 'DistanceWeight'));
asserts.checkerror('fitcknn([1; 2], [1; 2], ''Standardize'', 2)', sprintf(_('%s must be true or false.'), 'Standardize'));
asserts.checkerror('fitcknn([1 2; 3 4], [1; 2], ''Scale'', [1 0])', _('Scale must be a positive finite vector with one value per predictor.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
