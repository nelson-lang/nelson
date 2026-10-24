%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function outcome = run_normalizeStatus(testCase) %#ok<NLS0026>
  if isfield(testCase, 'timed_out') && testCase.timed_out
    outcome = 'timeout';
  elseif testCase.isbench && ~strcmp(testCase.status, 'Fail')
    outcome = 'bench';
  elseif strcmp(testCase.status, 'Pass')
    outcome = 'passed';
  elseif strcmp(testCase.status, 'Fail')
    if run_exitCode(testCase) ~= 0 && run_exitCode(testCase) ~= 1
      outcome = 'error';
    else
      outcome = 'failed';
    end
  else
    outcome = 'skipped';
  end
end
