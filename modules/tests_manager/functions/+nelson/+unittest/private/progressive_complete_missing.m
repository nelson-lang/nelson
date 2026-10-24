%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function updatedState = progressive_complete_missing(currentState)
  updatedState = currentState;
  missingIndices = find(~updatedState.completed);
  for caseIndex = missingIndices(:)'
    nativeResult = emptyNativeResults(1);
    nativeResult.exit_code = 1;
    nativeResult.reason = 'missing_result';
    nativeResult.stderr = 'Native process did not return a result.';
    nativeResult.index = caseIndex;
    nativeResult.metadata = updatedState.cases(caseIndex).filename;
    nativeResult.job_id = ['case_', int2str(caseIndex)];
    testCase = run_progressive_direct_case(updatedState.cases(caseIndex), ...
      nativeResult, launcher_aborted_code());
    if updatedState.fallback(caseIndex)
      testCase.worker_pool_used = true;
      testCase.worker_pool_fallback = true;
      testCase.worker_pool_failure = updatedState.failures{caseIndex};
    end
    updatedState.results{caseIndex} = testCase;
    updatedState.completed(caseIndex) = true;
  end
end
%=============================================================================
