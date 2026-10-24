%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function updatedState = progressive_record_pid(currentState, event)
  updatedState = currentState;
  if event.pid > 0 && ~any(updatedState.pids == event.pid)
    updatedState.pids = [updatedState.pids, event.pid];
  end
end
%=============================================================================
