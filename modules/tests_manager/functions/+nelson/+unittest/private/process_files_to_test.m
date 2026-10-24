%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = process_files_to_test(...
    test_files_list, classname, module_name, nbWorkers, runtimeOptions, stoponfail)
  test_suite = create_test_suite();
  test_suite.name = module_name;
  nbTests = length(test_files_list);
  if nbTests == 0
    fprintf(stdout, ['  ', _('No tests available.'), newline]);
  end
  display_preparation_message('prepare', nbTests, nbWorkers);
  preparationBegin = time();
  %#ok<NLS0004>
  [parallel_tests, sequential_tests, benchmark_tests] = partition_test_files(...
    test_files_list, classname, runtimeOptions, _('Preparing tests')); %#ok<NLS0004>
  tests_manager_trace('run', ['partition files=', int2str(nbTests), ...
    ' parallel=', int2str(length(parallel_tests)), ...
    ' sequential=', int2str(length(sequential_tests)), ...
    ' bench=', int2str(length(benchmark_tests)), ...
    ' elapsed=', sprintf('%.3fs', time() - preparationBegin)]);
  executionBegin = time();
  test_cases = process_partitioned_test_cases(...
    parallel_tests, sequential_tests, benchmark_tests, ...
    nbWorkers, runtimeOptions, stoponfail); %#ok<NLS0004>
  executionTime = time() - executionBegin;
  tests_manager_trace('run', ['execution elapsed=', sprintf('%.3fs', executionTime)]);
  test_suite = fill_test_suite_from_cases(test_suite, test_cases, executionTime);
end
%=============================================================================
