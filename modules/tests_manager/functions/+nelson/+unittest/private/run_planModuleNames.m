%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function modules = run_planModuleNames(cases)
  modules = {};
  for k = 1:length(cases)
    moduleName = run_planSuiteName(cases(k).module);
    if ~any(strcmp(modules, moduleName))
      modules = [modules; moduleName];
    end
  end
end
%=============================================================================
