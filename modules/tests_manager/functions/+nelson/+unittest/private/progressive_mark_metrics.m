%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCases = progressive_mark_metrics(testCases, state, nbWorkers)
  if isempty(testCases)
    return
  end
  testCases(1).worker_pool_native_jobs = length(state.pids);
  testCases(1).worker_pool_requested_workers = nbWorkers;
  testCases(1).worker_pool_inline_jobs = 0;
  testCases(1).worker_pool_script_jobs = any([testCases.worker_pool_used]);
end
%=============================================================================
