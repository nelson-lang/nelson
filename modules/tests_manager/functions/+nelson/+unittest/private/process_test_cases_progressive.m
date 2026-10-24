%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCasesUpdated = process_test_cases_progressive(...
    testCases, nbWorkers, initialIndex, nbTotalTests, stoponfail)
  phaseBegin = time();
  testCases = sort_test_cases_by_order(testCases);
  state = progressive_initial_state(testCases);
  fmtStart = test_start_format(nbTotalTests);
  allIndices = 1:length(testCases);
  [jobs, state.cases, worker] = progressive_test_jobs(...
    state.cases, allIndices, 1, _('Scheduling jobs'));
  tests_manager_trace('run', ['job preparation cases=', int2str(length(testCases)), ...
    ' elapsed=', sprintf('%.3fs', time() - phaseBegin)]);
  tests_manager_trace('progressive', ['start cases=', int2str(length(testCases)), ...
    ' jobs=', int2str(length(jobs.index)), ' workers=', int2str(nbWorkers)]);
  if isempty(jobs.index)
    state = progressive_flush_ready(state, initialIndex, nbTotalTests, fmtStart, stoponfail);
    testCasesUpdated = progressive_completed_cases(state);
    testCasesUpdated = progressive_mark_metrics(testCasesUpdated, state, nbWorkers);
    testCasesUpdated = remove_test_case_runtime_fields(testCasesUpdated);
    return
  end
  previousOmpThreads = configure_child_thread_environment(nbWorkers);
  try
    [state, stopped] = progressive_run_jobs(state, jobs, nbWorkers, true, ...
      initialIndex, nbTotalTests, fmtStart, stoponfail);
    run_removeFile(worker.command_filename);
    if ~stopped
      fallback = find(state.fallback & ~state.completed);
      if ~isempty(fallback)
        [fallbackJobs, state.cases, fallbackWorker] = ...
          progressive_test_jobs(state.cases, fallback, 2); %#ok<ASGLU>
        state.fallback_attempt(fallback) = true;
        [state, stopped] = progressive_run_jobs(state, fallbackJobs, ...
          min(nbWorkers, length(fallback)), false, initialIndex, ...
          nbTotalTests, fmtStart, stoponfail);
      end
    end
    if ~stopped
      state = progressive_complete_missing(state);
      [state, stopped] = progressive_flush_ready(state, initialIndex, ...
        nbTotalTests, fmtStart, stoponfail); %#ok<ASGLU>
    end
  catch ex
    restore_child_thread_environment(previousOmpThreads);
    progressive_cleanup_scripts(state.cases, worker);
    rethrow(ex);
  end
  restore_child_thread_environment(previousOmpThreads);
  progressive_cleanup_scripts(state.cases, worker);
  phaseBegin = time();
  testCasesUpdated = progressive_completed_cases(state);
  testCasesUpdated = progressive_mark_metrics(testCasesUpdated, state, nbWorkers);
  testCasesUpdated = remove_test_case_runtime_fields(testCasesUpdated);
  tests_manager_trace('run', ['result finalization cases=', ...
    int2str(length(testCasesUpdated)), ' elapsed=', ...
    sprintf('%.3fs', time() - phaseBegin)]);
end
%=============================================================================
% Child test processes inherit this environment: cap their computational
% thread pool so that nbWorkers concurrent children do not oversubscribe
% the machine (nbWorkers x physical cores threads otherwise).
function previousOmpThreads = configure_child_thread_environment(nbWorkers)
  previousOmpThreads = getenv('OMP_NUM_THREADS');
  childThreads = max(1, floor(maxNumCompThreads() / max(1, nbWorkers)));
  setenv('OMP_NUM_THREADS', int2str(childThreads));
end
%=============================================================================
function restore_child_thread_environment(previousOmpThreads)
  if isempty(previousOmpThreads)
    unsetenv('OMP_NUM_THREADS');
  else
    setenv('OMP_NUM_THREADS', previousOmpThreads);
  end
end
%=============================================================================
