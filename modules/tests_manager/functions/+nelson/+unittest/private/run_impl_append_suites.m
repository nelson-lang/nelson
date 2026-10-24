%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suites = run_impl_append_suites(test_suites, test_suite)
  for t = test_suite
    test_suites.disabled = test_suites.disabled + t.disabled;
    test_suites.errors = test_suites.errors + t.errors;
    test_suites.passed = test_suites.passed + t.passed;
    test_suites.tests = test_suites.tests + t.tests;
    test_suites.bench = test_suites.bench + t.bench;
    test_suites.time = test_suites.time + t.time;
    test_suites.worker_pool = worker_pool_metrics_merge(test_suites.worker_pool, t.worker_pool);
    test_suites.tests_list = [test_suites.tests_list; t];
  end
end
%=============================================================================
