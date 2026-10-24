%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('fitcensemble'), -3);
asserts.isequal(nargout('fitcensemble'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6; 10 0; 10 1; 11 0; 11 1];
Y = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3];
mdl = fitcensemble(X, Y, 'NumLearningCycles', 5, 'MaxNumSplits', 4, ...
  'MinLeafSize', 2, 'MinParentSize', 4);
asserts.isequal(class(mdl), 'ClassificationEnsemble');
asserts.isequal(mdl.Method, 'Bag');
asserts.isequal(mdl.LearnerTemplate, 'tree');
asserts.isequal(mdl.NumLearningCycles, 5);
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.ClassNames, [1; 2; 3]);
asserts.isequal(length(mdl.Learners), 5);
[label, score] = predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 3]);
asserts.isequal(score, eye(3));
%=============================================================================
stump = fitcensemble(X, Y, 'NumLearningCycles', 3, 'MaxNumSplits', 1);
asserts.isequal(length(stump.Learners), 3);
[label, score] = predict(stump, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 2]);
asserts.isapprox(score, [1 0 0; 0 0.5 0.5; 0 0.5 0.5], 1e-12);
%=============================================================================
species = {'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'virginica'; 'virginica'};
X2 = [1 1; 1 2; 4 4; 4 5; 8 8; 8 9];
mt = fitcensemble(X2, species, 'ClassNames', {'virginica'; 'versicolor'; 'setosa'}, ...
  'NumLearningCycles', 3, 'MinParentSize', 2);
[label, score] = predict(mt, [7.9 8.1; 1.1 1.2; 4.2 4.4]);
asserts.isequal(mt.ClassNames, {'virginica'; 'versicolor'; 'setosa'});
asserts.isequal(label, {'virginica'; 'setosa'; 'versicolor'});
asserts.isequal(score, [1 0 0; 0 0 1; 0 1 0]);
S = string(species);
ms = fitcensemble(X2, S, 'NumLearningCycles', 3, 'MinParentSize', 2);
asserts.isequal(predict(ms, [1.1 1.2; 4.2 4.4; 7.9 8.1]), ...
  ["setosa"; "versicolor"; "virginica"]);
%=============================================================================
asserts.checkerror('fitcensemble()', _('Not enough input arguments.'));
asserts.checkerror('fitcensemble({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitcensemble([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitcensemble([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitcensemble([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitcensemble(X, Y, ''Method'', ''bad'')', sprintf(_('Invalid %s value.'), 'Method'));
asserts.checkerror('fitcensemble(X, Y, ''Learners'', ''bad'')', sprintf(_('Invalid %s value.'), 'Learners'));
asserts.checkerror('fitcensemble(X, Y, ''NumLearningCycles'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'NumLearningCycles'));
asserts.checkerror('fitcensemble(X, Y, ''MaxNumSplits'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'MaxNumSplits'));
asserts.checkerror('fitcensemble(X, Y, ''MinLeafSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinLeafSize'));
asserts.checkerror('fitcensemble(X, Y, ''MinParentSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinParentSize'));
asserts.checkerror('fitcensemble(X, Y, ''ClassNames'', [1; 1; 2])', _('ClassNames must contain unique labels.'));
asserts.checkerror('fitcensemble(X, Y, ''ClassNames'', [1; 2])', _('ClassNames must include all observed classes.'));
asserts.checkerror('fitcensemble(X, Y, ''M'', ''Bag'')', _('Ambiguous name-value argument name.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
