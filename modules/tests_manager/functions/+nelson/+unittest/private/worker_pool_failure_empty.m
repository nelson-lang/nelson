%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function failure = worker_pool_failure_empty()
  failure = struct('missing_result', false, 'exit_code', 0, 'timed_out', false, ...
    'reason', '', 'job_id', '', 'pid', 0, 'stdout', '', 'stderr', '');
end
%=============================================================================
