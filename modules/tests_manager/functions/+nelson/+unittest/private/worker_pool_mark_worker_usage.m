%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_cases = worker_pool_mark_worker_usage(...
    test_cases, workers, requestedWorkers, nativeJobs)
  if nargin < 4
    nativeJobs = length(workers);
  end
  if isempty(test_cases)
    return
  end
  scriptJobs = worker_pool_count_script_jobs(workers);
  test_cases(1).worker_pool_native_jobs = nativeJobs;
  test_cases(1).worker_pool_requested_workers = requestedWorkers;
  test_cases(1).worker_pool_inline_jobs = length(workers) - scriptJobs;
  test_cases(1).worker_pool_script_jobs = scriptJobs;
end
%=============================================================================
function count = worker_pool_count_script_jobs(workers)
  count = 0;
  for k = 1:length(workers)
    if ~isempty(workers(k).command_filename)
      count = count + 1;
    end
  end
end
%=============================================================================
