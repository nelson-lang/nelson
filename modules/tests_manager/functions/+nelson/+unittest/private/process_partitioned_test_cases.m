%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCases = process_partitioned_test_cases(...
    parallelTests, sequentialTests, benchmarkTests, ...
    nbWorkers, runtimeOptions, stoponfail)
  total = length(parallelTests) + length(sequentialTests) + length(benchmarkTests);
  [parallelTests, parallelBugTests] = split_bug_test_cases(parallelTests);
  [sequentialTests, sequentialBugTests] = split_bug_test_cases(sequentialTests);
  % Announce the whole run once, before any batch executes. The tests are
  % executed in successive parallel/sequential/benchmark batches; printing this
  % banner per batch would make it appear in the middle of the results (after
  % the first batch already completed), which reads as a desynchronized display.
  display_preparation_message('run', total, nbWorkers);
  phaseBegin = time();
  parallel = process_test_cases(...
    parallelTests, nbWorkers, total, 1, stoponfail, runtimeOptions);
  tests_manager_trace('run', ['parallel elapsed=', ...
    sprintf('%.3fs', time() - phaseBegin)]);
  sequential = [];
  parallelBugs = [];
  sequentialBugs = [];
  benchmark = [];
  if ~(stoponfail && has_failed_cases(parallel))
    phaseBegin = time();
    sequential = process_test_cases(...
      sequentialTests, 1, total, length(parallel) + 1, stoponfail, runtimeOptions);
    tests_manager_trace('run', ['sequential elapsed=', ...
      sprintf('%.3fs', time() - phaseBegin)]);
  end
  if ~(stoponfail && has_failed_cases([parallel; sequential]))
    phaseBegin = time();
    parallelBugs = process_test_cases(...
      parallelBugTests, nbWorkers, total, length(parallel) + length(sequential) + 1, ...
      stoponfail, runtimeOptions);
    tests_manager_trace('run', ['parallel bugs elapsed=', ...
      sprintf('%.3fs', time() - phaseBegin)]);
  end
  if ~(stoponfail && has_failed_cases([parallel; sequential; parallelBugs]))
    phaseBegin = time();
    sequentialBugs = process_test_cases(...
      sequentialBugTests, 1, total, length(parallel) + length(sequential) ...
      + length(parallelBugs) + 1, stoponfail, runtimeOptions);
    tests_manager_trace('run', ['sequential bugs elapsed=', ...
      sprintf('%.3fs', time() - phaseBegin)]);
  end
  if ~(stoponfail && has_failed_cases([parallel; sequential; parallelBugs; sequentialBugs]))
    phaseBegin = time();
    benchmark = process_test_cases(benchmarkTests, ...
      benchmark_worker_count(nbWorkers), total, ...
      length(parallel) + length(sequential) + length(parallelBugs) ...
      + length(sequentialBugs) + 1, stoponfail, runtimeOptions);
    tests_manager_trace('run', ['bench elapsed=', ...
      sprintf('%.3fs', time() - phaseBegin)]);
  end
  phaseBegin = time();
  [parallel, sequential] = align_test_case_fields(parallel, sequential);
  [parallel, parallelBugs] = align_test_case_fields(parallel, parallelBugs);
  [parallel, sequentialBugs] = align_test_case_fields(parallel, sequentialBugs);
  [parallel, benchmark] = align_test_case_fields(parallel, benchmark);
  [sequential, parallelBugs] = align_test_case_fields(sequential, parallelBugs);
  [sequential, sequentialBugs] = align_test_case_fields(sequential, sequentialBugs);
  [sequential, benchmark] = align_test_case_fields(sequential, benchmark);
  [parallelBugs, sequentialBugs] = align_test_case_fields(parallelBugs, sequentialBugs);
  [parallelBugs, benchmark] = align_test_case_fields(parallelBugs, benchmark);
  [sequentialBugs, benchmark] = align_test_case_fields(sequentialBugs, benchmark);
  testCases = sort_test_cases_by_order([parallel; sequential; parallelBugs; ...
    sequentialBugs; benchmark]);
  tests_manager_trace('run', ['merge elapsed=', sprintf('%.3fs', time() - phaseBegin)]);
end
%=============================================================================
