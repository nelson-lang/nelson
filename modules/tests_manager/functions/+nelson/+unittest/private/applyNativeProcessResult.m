%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCase = applyNativeProcessResult(testCase, result)
  testCase.exit_code = result.exit_code;
  testCase.timed_out = result.timed_out;
  testCase.stderr = result.stderr;
  testCase = applyNativeProcessDiagnostics(testCase, result);
  if isfield(result, 'runfile_result') && isstruct(result.runfile_result)
    testCase.stdout = result.stdout;
    testCase.runfile_result = result.runfile_result;
    return
  end
  [payload, stdoutText] = extractRunfilePayload(result.stdout); %#ok<NLS0004>
  testCase.stdout = stdoutText;
  if ~isempty(payload)
    testCase.runfile_result = decode_runfile_payload(payload);
  end
end
