%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function results = run_normalizeResults(raw, plan, status)
  results = struct();
  results.type = 'TestRunResult';
  results.status = logical(status);
  results.cases = run_normalizeCases(raw);
  results.summary = struct();
  results.summary.passed = run_countNormalizedOutcome(results.cases, 'passed');
  results.summary.failed = run_countNormalizedOutcome(results.cases, 'failed');
  results.summary.skipped = run_countNormalizedOutcome(results.cases, 'skipped');
  results.summary.filtered = run_planFilteredCount(plan);
  results.summary.timeout = run_countNormalizedOutcome(results.cases, 'timeout');
  results.summary.error = run_countNormalizedOutcome(results.cases, 'error');
  results.summary.bench = raw.bench;
  results.summary.tests = raw.tests;
  results.summary.duration = raw.total_time;
  if isfield(raw, 'worker_pool')
    results.summary.worker_pool = raw.worker_pool;
  end
  results.plan = plan;
  results.raw = raw;
end
