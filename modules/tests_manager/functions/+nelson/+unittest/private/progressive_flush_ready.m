%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [updatedState, stopped] = progressive_flush_ready(currentState, ...
    initialIndex, nbTotalTests, fmtStart, stoponfail)
  updatedState = currentState;
  stopped = false;
  ready = updatedState.cursor <= length(updatedState.results);
  while ready && updatedState.completed(updatedState.cursor)
    testCase = updatedState.results{updatedState.cursor};
    displayIndex = initialIndex + updatedState.cursor - 1;
    display_progressive_case(testCase, displayIndex, nbTotalTests, fmtStart);
    updatedState.cursor = updatedState.cursor + 1;
    if stoponfail && strcmp(testCase.status, 'Fail')
      stopped = true;
      return
    end
    ready = updatedState.cursor <= length(updatedState.results);
  end
end
%=============================================================================
