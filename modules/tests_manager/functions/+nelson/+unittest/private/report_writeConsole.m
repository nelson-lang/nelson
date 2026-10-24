%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report_writeConsole(results, verbose)
  if isfield(results, 'summary')
    report_writeConsoleSummary(results, verbose);
    if verbose
      report_writeConsoleCases(report_resultCases(results));
    end
  else
    disp(results);
  end
end
