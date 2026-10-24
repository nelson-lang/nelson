%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[X, y] = statistics_fitrensemble_fixture();
mdl = fitrensemble(X, y, 'NumLearningCycles', 3, 'MaxNumSplits', 4, ...
  'PredictorNames', {'A'; 'B'}, 'ResponseName', 'Y');
asserts.isequal(mdl.PredictorNames, {'A'; 'B'});
asserts.isequal(mdl.ResponseName, 'Y');
%=============================================================================
