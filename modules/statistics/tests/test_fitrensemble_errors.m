%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
[X, y] = statistics_fitrensemble_fixture();
mdl = fitrensemble(X, y, 'NumLearningCycles', 5, 'MaxNumSplits', 4, ...
  'MinLeafSize', 2, 'MinParentSize', 4);
asserts.checkerror('fitrensemble()', _('Not enough input arguments.'));
asserts.checkerror('fitrensemble({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('fitrensemble([1; 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('fitrensemble([1; 2], [1])', sprintf(_('Y must be a vector with one %s per row of X.'), 'response'));
asserts.checkerror('fitrensemble(zeros(2,0), [1; 2])', _('X must contain at least one observation and one predictor.'));
asserts.checkerror('fitrensemble([1; Inf], [1; 2])', _('X and Y must contain finite values or NaN missing values.'));
asserts.checkerror('fitrensemble([NaN; NaN], [NaN; NaN])', _('No valid observations remain after removing missing values.'));
asserts.checkerror('fitrensemble(X, y, ''Method'', ''bad'')', sprintf(_('Invalid %s value.'), 'Method'));
asserts.checkerror('fitrensemble(X, y, ''Learners'', ''bad'')', sprintf(_('Invalid %s value.'), 'Learners'));
asserts.checkerror('fitrensemble(X, y, ''NumLearningCycles'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'NumLearningCycles'));
asserts.checkerror('fitrensemble(X, y, ''MaxNumSplits'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'MaxNumSplits'));
asserts.checkerror('fitrensemble(X, y, ''MinLeafSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinLeafSize'));
asserts.checkerror('fitrensemble(X, y, ''MinParentSize'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MinParentSize'));
asserts.checkerror('fitrensemble(X, y, ''PredictorNames'', {''A''})', _('PredictorNames must have one name per predictor.'));
asserts.checkerror('fitrensemble(X, y, ''ResponseName'', 1)', sprintf(_('%s must be a character vector or string scalar.'), 'ResponseName'));
asserts.checkerror('fitrensemble(X, y, ''M'', ''Bag'')', _('Ambiguous name-value argument name.'));
asserts.checkerror('predict(mdl, [1 2 3])', sprintf(_('X must have the same number of columns as %s.'), 'the training data'));
asserts.checkerror('predict(mdl, [1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
%=============================================================================
