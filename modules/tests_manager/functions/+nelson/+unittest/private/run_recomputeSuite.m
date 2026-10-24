%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function suite = run_recomputeSuite(suite) %#ok<NLS0026>
  suite.passed = 0;
  suite.errors = 0;
  suite.disabled = 0;
  suite.bench = 0;
  suite.tests = length(suite.test_cases_list);
  suite.time = 0;
  suite.worker_pool = worker_pool_metrics_from_cases(suite.test_cases_list);
  for k = 1:length(suite.test_cases_list)
    c = suite.test_cases_list(k);
    suite.time = suite.time + c.time;
    if c.isbench
      suite.bench = suite.bench + 1;
    end
    if strcmp(c.status, 'Pass') || (c.isbench && ~strcmp(c.status, 'Fail'))
      suite.passed = suite.passed + 1;
    elseif strcmp(c.status, 'Fail')
      suite.errors = suite.errors + 1;
    else
      suite.disabled = suite.disabled + 1;
    end
  end
end
