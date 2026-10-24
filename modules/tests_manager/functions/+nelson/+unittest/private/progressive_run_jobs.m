%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [updatedState, stopped] = progressive_run_jobs(currentState, jobs, ...
    nbWorkers, allowFallback, initialIndex, nbTotalTests, fmtStart, stoponfail)
  updatedState = currentState;
  stopped = false;
  if isempty(jobs.index)
    [updatedState, stopped] = progressive_flush_ready(updatedState, initialIndex, ...
      nbTotalTests, fmtStart, stoponfail);
    return
  end
  handle = __nelson_unittest_process_supervisor_start__(jobs, nbWorkers);
  try
    done = false;
    while ~done && ~stopped
      polledEvents = __nelson_unittest_process_supervisor_poll__(handle, 20);
      if ~isempty(polledEvents)
        tests_manager_trace('events', ...
          ['poll events=', int2str(length(polledEvents))]);
      end
      [updatedState, done] = progressive_consume_events(...
        updatedState, polledEvents, allowFallback);
      [updatedState, stopped] = progressive_flush_ready(updatedState, ...
        initialIndex, nbTotalTests, fmtStart, stoponfail);
    end
  catch ex
    __nelson_unittest_process_supervisor_stop__(handle);
    rethrow(ex);
  end
  __nelson_unittest_process_supervisor_stop__(handle);
end
%=============================================================================
