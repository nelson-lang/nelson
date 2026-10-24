%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [parallel_tests, sequential_tests, benchmark_tests] = partition_test_files(...
    test_files_list, classname, runtimeOptions, progressLabel)
  if nargin < 4
    progressLabel = '';
  end
  parallel_tests = [];
  sequential_tests = [];
  benchmark_tests = [];
  progress = progress_indicator_start(progressLabel, length(test_files_list), true);
  try
    for k = 1:length(test_files_list)
      test_case = create_test_case(test_files_list{k}, classname, runtimeOptions);
      test_case.classname = classname;
      test_case.test_order_index = k;
      if test_case.isbench
        benchmark_tests = [benchmark_tests, test_case];
      elseif needs_sequential_execution(test_case)
        sequential_tests = [sequential_tests, test_case];
      else
        parallel_tests = [parallel_tests, test_case];
      end
      progress = progress_indicator_update(progress, k);
    end
  catch ex
    progress_indicator_finish(progress, length(parallel_tests) ...
      + length(sequential_tests) + length(benchmark_tests));
    rethrow(ex);
  end
  progress_indicator_finish(progress);
end
%=============================================================================
