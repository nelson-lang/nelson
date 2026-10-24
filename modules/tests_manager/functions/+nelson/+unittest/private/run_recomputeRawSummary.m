%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function raw = run_recomputeRawSummary(raw)
  raw.passed = 0;
  raw.errors = 0;
  raw.disabled = 0;
  raw.bench = 0;
  raw.tests = 0;
  raw.time = 0;
  raw.worker_pool = worker_pool_metrics_empty();
  for s = 1:length(raw.tests_list)
    suite = run_recomputeSuite(raw.tests_list(s));
    raw.tests_list(s) = suite;
    raw.passed = raw.passed + suite.passed;
    raw.errors = raw.errors + suite.errors;
    raw.disabled = raw.disabled + suite.disabled;
    raw.bench = raw.bench + suite.bench;
    raw.tests = raw.tests + suite.tests;
    raw.time = raw.time + suite.time;
    raw.worker_pool = worker_pool_metrics_merge(raw.worker_pool, suite.worker_pool);
  end
end
