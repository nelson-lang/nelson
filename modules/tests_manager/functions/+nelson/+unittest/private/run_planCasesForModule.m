%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function selected = run_planCasesForModule(cases, moduleName)
  selected = struct([]);
  for k = 1:length(cases)
    caseModule = run_planSuiteName(cases(k).module);
    if strcmp(caseModule, moduleName)
      selected = [selected; cases(k)];
    end
  end
end
%=============================================================================
