%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function modules_list = getAllModulesList()
  modules_list = readAvailableModulesFromFile();
  current_modules = getmodules();
  if isempty(modules_list)
    modules_list = current_modules;
  end
  if ~isequal(modules_list, current_modules)
    for m = current_modules'
      if ~any(contains(modules_list, m{1}))
        modules_list = [modules_list; m{1}];
      end
    end
  end
  modules_list = removeUnavailableModules(modules_list);
end
