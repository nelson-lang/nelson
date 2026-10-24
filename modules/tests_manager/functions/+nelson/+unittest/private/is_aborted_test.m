%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = is_aborted_test(status, message, testCase, abortedCode)
  tf = false;
  if status == abortedCode
    tf = strcmp(message, 'ABORTED') || hasLauncherGrace(testCase);
  end
end
%=============================================================================
