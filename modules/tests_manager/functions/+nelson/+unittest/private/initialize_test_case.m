%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = initialize_test_case(filename, classname, runtimeOptions, parsedTags)
  test_case.filename = filename;
  if nargin < 4
    test_case.options = parsetags(filename);
  else
    test_case.options = parsedTags;
  end
  test_case.msg = '';
  [~, f, ~] = fileparts(filename); %#ok<NLS0004>
  test_case.name = f;
  test_case.classname = classname;
  test_case.status = '';
  test_case.time = 0;
  test_case.isbench = isbench(filename);
  test_case.skip = false;
  test_case.exit_code = 0;
  test_case.res_cmd = 0;
  test_case.timed_out = false;
  test_case.stdout = '';
  test_case.stderr = '';
  test_case.native_kind = '';
  test_case.native_index = 0;
  test_case.native_metadata = '';
  test_case.native_job_id = '';
  test_case.native_pid = 0;
  test_case.native_timeout = 0;
  test_case.native_reason = '';
  test_case.native_process_policy = '';
  test_case.native_executable = '';
  test_case.native_process_arguments = '';
  test_case.timeout = defaultTimeout(test_case.isbench, runtimeOptions, ...
    option_timeout(test_case.options));
  test_case.launcher_timeout = launcherTimeout(test_case.timeout, runtimeOptions);
  test_case.command = 'echo skip';
  test_case.executable = '';
  test_case.process_arguments = '';
  test_case.outputfile = '';
  test_case.redirect_err = '';
  test_case.command_filename = '';
  test_case.runfile_result = [];
  test_case.pool_index = 0;
  test_case.pool_missing_result = false;
  test_case.test_order_index = 0;
  test_case.worker_pool_used = false;
  test_case.worker_pool_fallback = false;
  test_case.worker_pool_eligible = [];
  test_case.worker_pool_native_jobs = 0;
  test_case.worker_pool_requested_workers = 0;
  test_case.worker_pool_inline_jobs = 0;
  test_case.worker_pool_script_jobs = 0;
  test_case.worker_pool_failure = worker_pool_failure_empty();
  test_case.weight = test_case.options.weight;
end
%=============================================================================
