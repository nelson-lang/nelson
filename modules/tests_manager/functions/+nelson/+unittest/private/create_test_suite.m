%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = create_test_suite()
  test_suite.name = '';
  test_suite.tests = 0;
  test_suite.disabled = 0;
  test_suite.errors = 0;
  test_suite.passed = 0;
  test_suite.bench = 0;
  test_suite.hostname = hostname();
  test_suite.id = createGUID();
  test_suite.package = '';
  test_suite.skipped = 0;
  test_suite.time = 0;
  test_suite.timestamp = timestamp(); % 2017-01-21T16:17:18
  test_suite.environment = get_environment_test();
  test_suite.test_cases_list = struct([]);
  test_suite.worker_pool = worker_pool_metrics_empty();
end
