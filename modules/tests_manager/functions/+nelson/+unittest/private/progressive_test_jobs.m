%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [jobs, testCases, worker] = progressive_test_jobs(...
    testCases, indices, attempt, progressLabel)
  if nargin < 4
    progressLabel = '';
  end
  jobs = progressive_empty_jobs();
  worker = worker_pool_empty_worker();
  requestedIndices = indices(:)';
  progress = progress_indicator_start(progressLabel, length(requestedIndices), true);
  tests_manager_trace('jobs', ['build attempt=', int2str(attempt), ...
    ' indices=', mat2str(requestedIndices)]);
  reusable = progressive_reusable_indices(testCases, requestedIndices, attempt);
  if ~isempty(reusable)
    worker = progressive_create_reusable_worker(testCases(reusable(1)));
  end
  configuredIndices = [];
  configuredCases = [];
  count = 0;
  try
    for k = requestedIndices
      count = count + 1;
      tests_manager_trace('jobs', ['visit index=', mat2str(k), ...
        ' skip=', mat2str(testCases(k).skip)]);
      if ~testCases(k).skip
        tests_manager_trace('jobs', ['configure index=', mat2str(k)]);
        testCase = testCases(k);
        useWorker = attempt == 1 && can_pool_test_case(testCase);
        tests_manager_trace('jobs', ['reuse index=', mat2str(k), ...
          ' value=', mat2str(useWorker)]);
        if useWorker
          tests_manager_trace('jobs', 'reusable branch');
          executable = worker.executable;
          processArguments = worker.process_arguments;
          payload = testCase.filename;
          kind = progressive_case_kind(testCase);
          launcher = 'cli';
        else
          tests_manager_trace('jobs', 'direct branch');
          testCase = configure_test_case_launchers(testCase);
          configuredIndices = [configuredIndices, k];
          configuredCases = [configuredCases; testCase];
          executable = testCase.executable;
          processArguments = testCase.process_arguments;
          payload = '';
          kind = progressive_case_kind(testCase);
          launcher = progressive_case_launcher(testCase);
        end
        jobs = progressive_append_job(jobs, testCase, k, attempt, ...
          executable, processArguments, payload, kind, launcher, useWorker);
        tests_manager_trace('jobs', ['append index=', int2str(k), ...
          ' kind=', kind, ' launcher=', launcher]);
      end
      progress = progress_indicator_update(progress, count);
    end
  catch ex
    progress_indicator_finish(progress, count);
    rethrow(ex);
  end
  progress_indicator_finish(progress);
  if ~isempty(configuredIndices)
    testCases(configuredIndices) = configuredCases;
  end
  tests_manager_trace('jobs', ['ready count=', int2str(length(jobs.index))]);
end
%=============================================================================
