%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function outcome = report_caseOutcome(testCase)
  if isfield(testCase, 'outcome')
    outcome = testCase.outcome;
    return
  end
  if strcmp(testCase.status, 'Pass') || (testCase.isbench && ~strcmp(testCase.status, 'Fail'))
    outcome = 'passed';
  elseif strcmp(testCase.status, 'Fail')
    outcome = 'failed';
  else
    outcome = 'skipped';
  end
end
