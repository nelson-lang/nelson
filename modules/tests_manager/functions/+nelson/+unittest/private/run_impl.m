%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, test_suites] = run_impl(varargin)
  nbWorkers = maxNumCompThreads();
  cleanupOnExit = ~is_unittest_runfile_worker();
  try
    __totalTimeBegin = time();
    tests_manager_trace('run', 'start');
    [varargin, runtimeOptions] = extractRuntimeOptions(varargin); %#ok<NLS0004>
    [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
      test_run_parse_input_arguments(length(varargin), varargin);
    tests_manager_trace('run', ['arguments elapsed=', ...
      sprintf('%.3fs', time() - __totalTimeBegin)]);
    nelson_version = version('-number');
    nelson_version_str = sprintf('Nelson-%d.%d.%d.%d', nelson_version(1), ...
      nelson_version(2), nelson_version(3), nelson_version(4));
    if isempty(files_to_test)
      test_suites = run_impl_modules(...
        modules_list, option, nbWorkers, runtimeOptions, stoponfail, nelson_version_str);
    else
      test_suites = run_impl_files(...
        files_to_test, nbWorkers, runtimeOptions, stoponfail, nelson_version_str);
    end
    if cleanupOnExit
      run_cleanupProcesses(false);
    end
    tests_manager_trace('run', ['cleanup elapsed=', ...
      sprintf('%.3fs', time() - __totalTimeBegin)]);
    __totalTimeEnd = time();
    status = test_suites.errors == 0;
    test_suites.total_time = __totalTimeEnd - __totalTimeBegin;
    res = test_run_save_results(test_suites, result_outputfile);
    if ~res
      error('Nelson:tests_manager:cannotSaveResults', 'Impossible to save results file.');
    end
    test_run_disp_summary(test_suites);
  catch ex
    if cleanupOnExit
      run_cleanupProcesses(true);
    end
    rethrow(ex);
  end
end
