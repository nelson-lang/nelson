%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report_writeWorkerPoolSummary(metrics)
  if ~metrics.enabled
    return
  end
  fprintf(['worker pool: %d attempted, %d completed, %d fallback, %d direct, ', ...
    '%d native jobs, %d requested workers, %d inline jobs, %d script jobs\n'], ...
    metrics.attempted, metrics.completed, metrics.fallback, metrics.direct, ...
    metrics.native_jobs, metrics.requested_workers, metrics.inline_jobs, ...
    metrics.script_jobs);
end
%=============================================================================
