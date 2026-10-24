%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function merged = worker_pool_metrics_merge(left, right)
  merged = left;
  merged.enabled = left.enabled || right.enabled;
  merged.attempted = left.attempted + right.attempted;
  merged.completed = left.completed + right.completed;
  merged.fallback = left.fallback + right.fallback;
  merged.direct = left.direct + right.direct;
  merged.native_jobs = left.native_jobs + right.native_jobs;
  merged.requested_workers = max(left.requested_workers, right.requested_workers);
  merged.inline_jobs = left.inline_jobs + right.inline_jobs;
  merged.script_jobs = left.script_jobs + right.script_jobs;
end
%=============================================================================
