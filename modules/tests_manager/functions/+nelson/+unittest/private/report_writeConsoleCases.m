%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report_writeConsoleCases(cases)
  for k = 1:length(cases)
    line = ['  ', report_caseOutcome(cases(k)), ' ', report_caseName(cases(k)), ...
      ' (', report_durationText(report_caseDuration(cases(k))), 's)'];
    fprintf('%s\n', line);
    message = report_oneLine(report_caseMessage(cases(k)));
    if ~isempty(message)
      fprintf('    %s\n', message);
    end
  end
end
