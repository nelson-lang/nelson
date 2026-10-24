%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function state = progressive_initial_state(testCases)
  caseCount = length(testCases);
  state.cases = testCases;
  state.results = cell(1, caseCount);
  state.completed = false(1, caseCount);
  state.fallback = false(1, caseCount);
  state.fallback_attempt = false(1, caseCount);
  state.failures = cell(1, caseCount);
  state.cursor = 1;
  state.pids = [];
  for k = 1:caseCount
    if testCases(k).skip
      state.results{k} = testCases(k);
      state.completed(k) = true;
    end
  end
end
%=============================================================================
