%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = nmm_is_installed(module_name)
  modules = nmm_list();
  for n = fieldnames(modules)'
    if strcmp(n{1}, module_name)
      r = true;
      return;
    end
  end
  r = false;
end
%=============================================================================
