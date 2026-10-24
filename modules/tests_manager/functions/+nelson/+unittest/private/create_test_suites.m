%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suites = create_test_suites()
  test_suites.disabled = 0;
  test_suites.errors = 0;
  test_suites.name = '';
  test_suites.passed = 0;
  test_suites.tests = 0;
  test_suites.bench = 0;
  test_suites.time = 0;
  test_suites.tests_list = struct([]);
  test_suites.worker_pool = worker_pool_metrics_empty();
end
