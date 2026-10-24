%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = struct();
base.filename = '';
base.options = struct();
base.msg = '';
base.name = '';
base.classname = '';
base.status = 'Pass';
base.time = 1;
base.isbench = false;
base.skip = false;
base.exit_code = 0;
base.timed_out = false;
base.stdout = '';
base.stderr = '';
base.timeout = 0;
base.launcher_timeout = 0;
base.command = '';
base.executable = '';
base.process_arguments = '';
base.outputfile = '';
base.redirect_err = '';
base.command_filename = '';
base.runfile_result = [];
base.pool_index = 0;
base.pool_missing_result = false;
base.test_order_index = 0;
base.worker_pool_used = false;
base.worker_pool_fallback = false;
base.worker_pool_eligible = [];
base.worker_pool_native_jobs = 0;
base.worker_pool_requested_workers = 0;
base.worker_pool_inline_jobs = 0;
base.worker_pool_script_jobs = 0;
base.weight = 1;

cases = repmat(base, 1, 3);
for k = 1:length(cases)
  cases(k).name = ['case_', int2str(k)];
  cases(k).worker_pool_used = true;
end
cases(3).worker_pool_fallback = true;
cases(1).worker_pool_native_jobs = 2;
cases(1).worker_pool_requested_workers = 4;
cases(1).worker_pool_inline_jobs = 2;
cases(1).worker_pool_script_jobs = 0;

suite = create_test_suite();
suite.name = 'metrics';
suite = fill_test_suite_from_cases(suite, cases);

raw = create_test_suites();
raw.tests_list = suite;
raw.total_time = 3;
raw = run_recomputeRawSummary(raw);

results = run_normalizeResults(raw, struct(), true);
metrics = results.summary.worker_pool;
asserts.istrue(metrics.enabled);
asserts.isequal(metrics.attempted, 3);
asserts.isequal(metrics.completed, 2);
asserts.isequal(metrics.fallback, 1);
asserts.isequal(metrics.direct, 1);
asserts.isequal(metrics.native_jobs, 2);
asserts.isequal(metrics.requested_workers, 4);
asserts.isequal(metrics.inline_jobs, 2);
asserts.isequal(metrics.script_jobs, 0);

txt = evalc('nelson.unittest.report(results, ''Format'', ''console'', ''Verbose'', true);');
asserts.istrue(contains(txt, 'worker pool: 3 attempted, 2 completed, 1 fallback, 1 direct'));
asserts.istrue(contains(txt, '2 native jobs, 4 requested workers'));
asserts.istrue(contains(txt, '2 inline jobs, 0 script jobs'));
%=============================================================================
