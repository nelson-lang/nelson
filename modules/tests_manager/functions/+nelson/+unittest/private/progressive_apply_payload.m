%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function updatedState = progressive_apply_payload(currentState, caseIndex, event)
  updatedState = currentState;
  payload = decode_runfile_payload(event.payload);
  testCase = worker_pool_apply_payload(...
    updatedState.cases(caseIndex), payload, event);
  if event.reusable || updatedState.fallback_attempt(caseIndex)
    testCase.worker_pool_used = true;
  end
  if updatedState.fallback_attempt(caseIndex)
    testCase.worker_pool_fallback = true;
    testCase.worker_pool_failure = updatedState.failures{caseIndex};
  end
  updatedState.results{caseIndex} = testCase;
  updatedState.completed(caseIndex) = true;
end
%=============================================================================
