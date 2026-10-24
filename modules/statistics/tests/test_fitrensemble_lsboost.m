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
boost = fitrensemble(X, y, 'Method', 'LSBoost', 'NumLearningCycles', 4, ...
  'LearnRate', 0.2, 'MaxNumSplits', 2, 'MinParentSize', 4);
asserts.isequal(boost.Method, 'LSBoost');
asserts.isequal(boost.NumLearningCycles, 4);
asserts.isequal(boost.LearnRate, 0.2);
asserts.isequal(length(boost.LearnerWeights), 4);
asserts.istrue(boost.ResubstitutionLoss < var(y));
legacy = fitensemble(X, y, 'LSBoost', 4, 'Tree', 'Type', 'regression', ...
  'LearnRate', 0.2, 'MaxNumSplits', 2, 'MinParentSize', 4);
asserts.isequal(class(legacy), 'RegressionEnsemble');
asserts.isequal(legacy.Method, 'LSBoost');
asserts.isequal(predict(compact(legacy), X), predict(legacy, X));
%=============================================================================
