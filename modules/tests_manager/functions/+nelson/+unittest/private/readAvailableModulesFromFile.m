%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function list = readAvailableModulesFromFile()
  run([nelsonroot(), '/modules/modules.m']);
  funcList = @(x) x{1};
  list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
  funcAvailable = @(x) x{2};
  available = cellfun(funcAvailable, modules_list, 'UniformOutput', true);
  list = list(available);
end
