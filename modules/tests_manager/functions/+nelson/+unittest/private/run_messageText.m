%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function message = run_messageText(testCase)
  message = '';
  if ~isfield(testCase, 'msg')
    return
  end
  if isstruct(testCase.msg) && isfield(testCase.msg, 'message')
    message = testCase.msg.message;
  elseif ischar(testCase.msg)
    message = testCase.msg;
  elseif iscellstr(testCase.msg)
    message = strjoin(testCase.msg, newline);
  end
end
