%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metrics = worker_pool_metrics_empty()
  metrics.enabled = false;
  metrics.attempted = 0;
  metrics.completed = 0;
  metrics.fallback = 0;
  metrics.direct = 0;
  metrics.native_jobs = 0;
  metrics.requested_workers = 0;
  metrics.inline_jobs = 0;
  metrics.script_jobs = 0;
end
%=============================================================================
