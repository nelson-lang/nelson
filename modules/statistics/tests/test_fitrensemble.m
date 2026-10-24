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
asserts.isequal(nargin('fitrensemble'), -3);
asserts.isequal(nargout('fitrensemble'), 1);
%=============================================================================
[X, y] = statistics_fitrensemble_fixture();
mdl = fitrensemble(X, y, 'NumLearningCycles', 5, 'MaxNumSplits', 4, ...
  'MinLeafSize', 2, 'MinParentSize', 4);
asserts.isequal(class(mdl), 'RegressionEnsemble');
asserts.isequal(mdl.Method, 'Bag');
asserts.isequal(mdl.LearnerTemplate, 'tree');
asserts.isequal(mdl.NumLearningCycles, 5);
asserts.isequal(mdl.NumObservations, 12);
asserts.isequal(mdl.NumPredictors, 2);
asserts.isequal(length(mdl.Learners), 5);
asserts.isapprox(mdl.ResubstitutionLoss, 1.143599999999998, 1e-12);
asserts.isapprox(predict(mdl, [0.2 0.2; 5.2 5.2; 10.2 0.2]), ...
  [1.16; 8.4; 20.14], 1e-12);
%=============================================================================
