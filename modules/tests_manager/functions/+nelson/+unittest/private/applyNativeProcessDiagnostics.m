%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function testCase = applyNativeProcessDiagnostics(testCase, result)
  testCase.native_kind = nativeDiagnosticField(result, 'kind', '');
  testCase.native_index = nativeDiagnosticField(result, 'index', 0);
  testCase.native_metadata = nativeDiagnosticField(result, 'metadata', '');
  testCase.native_job_id = nativeDiagnosticField(result, 'job_id', '');
  testCase.native_pid = nativeDiagnosticField(result, 'pid', 0);
  testCase.native_timeout = nativeDiagnosticField(result, 'timeout', 0);
  testCase.native_reason = nativeDiagnosticField(result, 'reason', '');
  testCase.native_process_policy = nativeDiagnosticField(result, 'process_policy', '');
  testCase.native_executable = nativeDiagnosticField(result, 'executable', '');
  testCase.native_process_arguments = nativeDiagnosticField(...
    result, 'process_arguments', '');
end
%=============================================================================
function value = nativeDiagnosticField(result, name, defaultValue)
  value = defaultValue;
  if isfield(result, name)
    value = result.(name);
  end
end
%=============================================================================
