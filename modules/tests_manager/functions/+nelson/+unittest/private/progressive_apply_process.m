%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function updatedState = progressive_apply_process(...
    currentState, caseIndex, event, allowFallback)
  updatedState = currentState;
  reusableFailure = allowFallback && event.reusable ...
    && ~event.timed_out && ~strcmp(event.reason, 'interrupted');
  if reusableFailure
    updatedState.fallback(caseIndex) = true;
    updatedState.failures{caseIndex} = worker_pool_failure_from_native(event);
    return
  end
  nativeResult = run_processes_progressive_event_to_native(event);
  testCase = run_progressive_direct_case(updatedState.cases(caseIndex), ...
    nativeResult, launcher_aborted_code());
  if testCase.time <= 0 && nativeResult.duration > 0
    testCase.time = double(nativeResult.duration) * 1e-3;
  end
  if event.reusable || updatedState.fallback_attempt(caseIndex)
    testCase.worker_pool_used = true;
    testCase.worker_pool_failure = worker_pool_failure_from_native(event);
  end
  if updatedState.fallback_attempt(caseIndex)
    testCase.worker_pool_fallback = true;
    testCase.worker_pool_failure = updatedState.failures{caseIndex};
  end
  updatedState.results{caseIndex} = testCase;
  updatedState.completed(caseIndex) = true;
end
%=============================================================================
