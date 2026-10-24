%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [status, message, testCase] = test_case_native_status(testCase, nativeResult)
  status = 0;
  message = '';
  if strcmp(testCase.command, 'echo skip')
    return
  end
  testCase = applyNativeProcessResult(testCase, nativeResult);
  status = nativeResult.exit_code;
  message = nativeRunMessage(nativeResult);
  if isfield(testCase, 'runfile_result') && isstruct(testCase.runfile_result)
    status = testCase.runfile_result.r;
    if isfield(testCase.runfile_result, 'msg')
      message = testCase.runfile_result.msg;
    end
  end
end
%=============================================================================
