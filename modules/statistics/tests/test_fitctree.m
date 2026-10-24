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
asserts.isequal(nargin('fitctree'), -3);
asserts.isequal(nargout('fitctree'), 1);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 5 5; 5 6; 6 5; 6 6; 10 0; 10 1; 11 0; 11 1];
Y = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3];
mdl = fitctree(X, Y, 'MaxNumSplits', 4, 'MinLeafSize', 2, 'MinParentSize', 4);
asserts.isequal(class(mdl), 'ClassificationTree');
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(mdl.ClassNames, [1; 2; 3]);
asserts.isequal(mdl.Prior, [1 / 3 1 / 3 1 / 3]);
asserts.isequal(sum(mdl.IsBranch), 2);
[label, score, cost, node] = predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 3]);
asserts.isequal(score, eye(3));
asserts.isequal(cost, ones(3) - eye(3));
asserts.isequal(size(node), [3 1]);
%=============================================================================
stump = fitctree(X, Y, 'MaxNumSplits', 1);
asserts.isequal(sum(stump.IsBranch), 1);
[label, score] = predict(stump, [0.2 0.2; 5.2 5.2; 10.2 0.2]);
asserts.isequal(label, [1; 2; 2]);
asserts.isapprox(score, [1 0 0; 0 0.5 0.5; 0 0.5 0.5], 1e-12);
%=============================================================================
mdl = fitctree(X, Y, 'SplitCriterion', 'deviance', 'Prior', 'uniform', 'MinParentSize', 4);
asserts.isequal(mdl.SplitCriterion, 'deviance');
asserts.isequal(mdl.Prior, [1 / 3 1 / 3 1 / 3]);
label = predict(mdl, [0.8 0.8; 5.6 5.4; 10.8 0.4]);
asserts.isequal(label, [1; 2; 3]);
mdl = fitctree(X, Y, 'SplitCriterion', 'twoing', 'MaxNumSplits', 3, 'MinParentSize', 4);
asserts.isequal(mdl.SplitCriterion, 'twoing');
label = predict(mdl, [0.8 0.8; 5.6 5.4; 10.8 0.4]);
asserts.isequal(label, [1; 2; 3]);
%=============================================================================
species = {'setosa'; 'setosa'; 'setosa'; 'setosa'; 'versicolor'; 'versicolor'; 'versicolor'; 'versicolor'};
X2 = [1 1; 1 2; 2 1; 2 2; 5 5; 5 6; 6 5; 6 6];
mdl = fitctree(X2, species, 'ClassNames', {'versicolor'; 'setosa'}, 'MinParentSize', 2);
[label, score] = predict(mdl, [5.2 5.1; 1.1 1.2]);
asserts.isequal(label, {'versicolor'; 'setosa'});
asserts.isequal(score, [1 0; 0 1]);
asserts.isequal(mdl.ClassNames, {'versicolor'; 'setosa'});
%=============================================================================
S = string(species);
mdl = fitctree(X2, S, 'MinParentSize', 2);
label = predict(mdl, [1.4 1.3; 5.4 5.3]);
asserts.isequal(label, ["setosa"; "versicolor"]);
%=============================================================================
asserts.checkerror('fitctree()', _('Not enough input arguments.'));
asserts.checkerror('fitctree({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitctree([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'label'));
asserts.checkerror('fitctree([1; 2], [1; 1])', _('Y must contain at least two classes.'));
asserts.checkerror('fitctree([1; Inf], [1; 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitctree(X, Y, ''SplitCriterion'', ''bad'')', sprintf(_('Invalid %s value.'), 'SplitCriterion'));
asserts.checkerror('fitctree(X, Y, ''Prior'', ''bad'')', sprintf(_('Invalid %s value.'), 'Prior'));
asserts.checkerror('fitctree(X, Y, ''MaxNumSplits'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'MaxNumSplits'));
asserts.checkerror('fitctree(X, Y, ''MinLeafSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinLeafSize'));
asserts.checkerror('fitctree(X, Y, ''MinParentSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinParentSize'));
asserts.checkerror('fitctree(X, Y, ''ClassNames'', [1; 1; 2])', _('ClassNames must contain unique labels.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
%=============================================================================
