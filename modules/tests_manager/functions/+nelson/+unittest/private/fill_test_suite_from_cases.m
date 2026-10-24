%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = fill_test_suite_from_cases(test_suite, test_cases, executionTime)
  for test_case = test_cases(:)'
    test_suite = append_test_case_summary(test_suite, test_case);
  end
  if nargin < 3
    test_suite.time = sum([test_cases.time]);
  else
    test_suite.time = executionTime;
  end
  test_suite.worker_pool = worker_pool_metrics_from_cases(test_cases);
  test_suite.test_cases_list = test_cases;
end
%=============================================================================
