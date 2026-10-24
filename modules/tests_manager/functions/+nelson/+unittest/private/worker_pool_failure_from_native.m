%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function failure = worker_pool_failure_from_native(nativeResult)
  failure = worker_pool_failure_empty();
  failure.missing_result = true;
  failure.exit_code = worker_pool_native_field(nativeResult, 'exit_code', 0);
  failure.timed_out = logical(worker_pool_native_field(nativeResult, 'timed_out', false));
  failure.reason = worker_pool_native_field(nativeResult, 'reason', '');
  failure.job_id = worker_pool_native_field(nativeResult, 'job_id', '');
  failure.pid = worker_pool_native_field(nativeResult, 'pid', 0);
  failure.stdout = worker_pool_native_field(nativeResult, 'stdout', '');
  failure.stderr = worker_pool_native_field(nativeResult, 'stderr', '');
end
%=============================================================================
function value = worker_pool_native_field(nativeResult, name, defaultValue)
  value = defaultValue;
  if isfield(nativeResult, name)
    value = nativeResult.(name);
  end
end
%=============================================================================
