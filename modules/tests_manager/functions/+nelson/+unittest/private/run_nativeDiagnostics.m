%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function diagnostics = run_nativeDiagnostics(testCase)
  diagnostics = struct();
  diagnostics.kind = run_nativeDiagnosticField(testCase, 'native_kind', '');
  diagnostics.index = run_nativeDiagnosticField(testCase, 'native_index', 0);
  diagnostics.metadata = run_nativeDiagnosticField(testCase, 'native_metadata', '');
  diagnostics.job_id = run_nativeDiagnosticField(testCase, 'native_job_id', '');
  diagnostics.pid = run_nativeDiagnosticField(testCase, 'native_pid', 0);
  diagnostics.timeout = run_nativeDiagnosticField(testCase, 'native_timeout', 0);
  diagnostics.reason = run_nativeDiagnosticField(testCase, 'native_reason', '');
  diagnostics.process_policy = run_nativeDiagnosticField(...
    testCase, 'native_process_policy', '');
  diagnostics.executable = run_nativeDiagnosticField(testCase, 'native_executable', '');
  diagnostics.process_arguments = run_nativeDiagnosticField(...
    testCase, 'native_process_arguments', '');
  diagnostics.worker_pool = run_workerPoolDiagnostics(testCase);
end
%=============================================================================
function value = run_nativeDiagnosticField(testCase, name, defaultValue)
  value = defaultValue;
  if isfield(testCase, name)
    value = testCase.(name);
  end
end
%=============================================================================
function workerPool = run_workerPoolDiagnostics(testCase)
  workerPool = struct();
  workerPool.used = logical(run_nativeDiagnosticField(testCase, 'worker_pool_used', false));
  workerPool.fallback = logical(run_nativeDiagnosticField(...
    testCase, 'worker_pool_fallback', false));
  workerPool.missing_result = logical(run_nativeDiagnosticField(...
    testCase, 'pool_missing_result', false));
  workerPool.failure = worker_pool_failure_empty();
  if isfield(testCase, 'worker_pool_failure')
    workerPool.failure = testCase.worker_pool_failure;
  end
end
%=============================================================================
