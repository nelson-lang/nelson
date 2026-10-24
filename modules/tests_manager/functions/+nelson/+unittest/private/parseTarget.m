%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [modules_list, files_to_test] = parseTarget(param)
  if isequal(size(param), [0 0]) && isdouble(param)
    modules_list = getAllModulesList();
    files_to_test = [];
  else
    modules_list = getModulesToTest(param);
    files_to_test = getFilesToTest(param);
  end
end
