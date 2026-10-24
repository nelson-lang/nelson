%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_cleaned = deep_copy_test_case(test_case_res)
  test_case_cleaned.filename = test_case_res.filename;
  test_case_cleaned.options = test_case_res.options;
  test_case_cleaned.msg = test_case_res.msg;
  test_case_cleaned.name = test_case_res.name;
  test_case_cleaned.classname = test_case_res.classname;
  test_case_cleaned.status = test_case_res.status;
  test_case_cleaned.time = test_case_res.time;
  test_case_cleaned.isbench = test_case_res.isbench;
  test_case_cleaned.skip = test_case_res.skip;
  test_case_cleaned.timeout = test_case_res.timeout;
  test_case_cleaned.launcher_timeout = test_case_res.launcher_timeout;
  test_case_cleaned.exit_code = test_case_res.exit_code;
  test_case_cleaned.res_cmd = test_case_res.res_cmd;
  test_case_cleaned.timed_out = test_case_res.timed_out;
  test_case_cleaned.stdout = test_case_res.stdout;
  test_case_cleaned.stderr = test_case_res.stderr;
  test_case_cleaned.native_kind = test_case_res.native_kind;
  test_case_cleaned.native_index = test_case_res.native_index;
  test_case_cleaned.native_metadata = test_case_res.native_metadata;
  test_case_cleaned.native_job_id = test_case_res.native_job_id;
  test_case_cleaned.native_pid = test_case_res.native_pid;
  test_case_cleaned.native_timeout = test_case_res.native_timeout;
  test_case_cleaned.native_reason = test_case_res.native_reason;
  test_case_cleaned.native_process_policy = test_case_res.native_process_policy;
  test_case_cleaned.native_executable = test_case_res.native_executable;
  test_case_cleaned.native_process_arguments = test_case_res.native_process_arguments;
  test_case_cleaned.runfile_result = test_case_res.runfile_result;
  test_case_cleaned.pool_index = test_case_res.pool_index;
  test_case_cleaned.pool_missing_result = test_case_res.pool_missing_result;
  test_case_cleaned.test_order_index = test_case_res.test_order_index;
  test_case_cleaned.worker_pool_used = test_case_res.worker_pool_used;
  test_case_cleaned.worker_pool_fallback = test_case_res.worker_pool_fallback;
  test_case_cleaned.worker_pool_eligible = test_case_res.worker_pool_eligible;
  test_case_cleaned.worker_pool_native_jobs = test_case_res.worker_pool_native_jobs;
  test_case_cleaned.worker_pool_requested_workers = test_case_res.worker_pool_requested_workers;
  test_case_cleaned.worker_pool_inline_jobs = test_case_res.worker_pool_inline_jobs;
  test_case_cleaned.worker_pool_script_jobs = test_case_res.worker_pool_script_jobs;
  test_case_cleaned.worker_pool_failure = test_case_res.worker_pool_failure;
  test_case_cleaned.weight = test_case_res.weight;
end
