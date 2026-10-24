%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function code = run_exitCode(testCase)
  code = 0;
  if isfield(testCase, 'exit_code')
    code = testCase.exit_code;
  elseif isfield(testCase, 'res_cmd')
    code = testCase.res_cmd;
  end
end
