%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suites = run_impl_files(...
    files_to_test, nbWorkers, runtimeOptions, stoponfail, nelson_version_str)
  test_suites = create_test_suites();
  test_suite = process_files_to_test(...
    files_to_test, nelson_version_str, '', nbWorkers, runtimeOptions, stoponfail);
  test_suites.disabled = test_suite.disabled;
  test_suites.errors = test_suite.errors;
  test_suites.passed = test_suite.passed;
  test_suites.tests = test_suite.tests;
  test_suites.bench = test_suite.bench;
  test_suites.time = test_suite.time;
  test_suites.worker_pool = test_suite.worker_pool;
  test_suites.tests_list = test_suite;
end
%=============================================================================
