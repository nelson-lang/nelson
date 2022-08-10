%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_uninstall(varargin)
  if (length(varargin) ~= 1)
    error(_('Wrong number of input arguments.'));
  end
  module_or_directory = varargin{1};
  if ~ischar(module_or_directory)
    error(_('#2 argument: characters expected.'));
  end
  [module_name, module_data] = nmm_find_installed_module(module_or_directory);
  if isempty(module_data)
    error(_('module name or existing directory installed is expected.'))
  end
  if ismodule(module_name)
    removemodule(module_name);
  end
  
  modified_modules = rmfield(nmm_list(), module_name);
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  txt = jsonprettyprint(jsonencode(modified_modules));
  filewrite(modules_json_path, txt);
  if startsWith(module_data.path, p)
    parent_dir = [module_data.path, '..'];
    d = dir(parent_dir);
    if length(d) == 3
      [r, msg] = rmdir(parent_dir, 's');
    else
      [r, msg] = rmdir(module_data.path, 's');
    end
  end
end
%=============================================================================
