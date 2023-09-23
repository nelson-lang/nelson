%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [module_name, module_data] = nmm_find_installed_module(module_or_path)
  module_name = [];
  module_data = struct([]);
  
  modules = nmm_list();
  if ~isempty(modules)
    if isfield(modules, module_or_path)
      module_name = module_or_path;
      module_data = getfield(modules, module_or_path);
    else
      if isdir(module_or_path)
        module_or_path = fullpath(module_or_path);
        if ~endsWith(module_or_path, '/')
          module_or_path = [module_or_path, '/'];
        end
        for m = fieldnames(modules)'
          s = getfield(modules, m{1});
          if strcmp(s.path, module_or_path)
            module_name = m{1};
            module_data = s;
            break;
          end
        end
      end
    end
  end
end
