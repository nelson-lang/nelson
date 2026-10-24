%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metrics = worker_pool_metrics_from_cases(test_cases)
  metrics = worker_pool_metrics_empty();
  nbCases = length(test_cases);
  metrics.direct = nbCases;
  for k = 1:nbCases
    metrics = worker_pool_metrics_count_case(metrics, test_cases(k));
    metrics = worker_pool_metrics_count_native(metrics, test_cases(k));
  end
  metrics.completed = metrics.attempted - metrics.fallback;
  metrics.direct = nbCases - metrics.completed;
  metrics.enabled = metrics.attempted > 0;
end
%=============================================================================
function metrics = worker_pool_metrics_count_case(metrics, test_case)
  if isfield(test_case, 'worker_pool_used') && test_case.worker_pool_used
    metrics.attempted = metrics.attempted + 1;
  end
  if isfield(test_case, 'worker_pool_fallback') && test_case.worker_pool_fallback
    metrics.fallback = metrics.fallback + 1;
  end
end
%=============================================================================
function metrics = worker_pool_metrics_count_native(metrics, test_case)
  if isfield(test_case, 'worker_pool_native_jobs')
    metrics.native_jobs = metrics.native_jobs + test_case.worker_pool_native_jobs;
  end
  if isfield(test_case, 'worker_pool_requested_workers')
    metrics.requested_workers = max(...
      metrics.requested_workers, test_case.worker_pool_requested_workers);
  end
  if isfield(test_case, 'worker_pool_inline_jobs')
    metrics.inline_jobs = metrics.inline_jobs + test_case.worker_pool_inline_jobs;
  end
  if isfield(test_case, 'worker_pool_script_jobs')
    metrics.script_jobs = metrics.script_jobs + test_case.worker_pool_script_jobs;
  end
end
%=============================================================================
