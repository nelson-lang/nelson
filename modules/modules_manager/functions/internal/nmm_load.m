%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_load(varargin)
  if nargin ~= 1
    error(_('Wrong number of input arguments.'));
  end
  [module_name, module_data] = nmm_find_installed_module(varargin{1});
  
  if isempty(module_data)
    error(_('module name or existing directory installed is expected.'))
  end
  if ~isfile([module_data.path, 'loader.m'])
    error(_('loader.m is missing.'));
  end
  if ~isfile([module_data.path, 'module.json'])
    error(_('module.json is missing.'));
  end
  if ~ismodule(module_name)
    run([module_data.path, 'loader.m']);
  else
    warning(sprintf(_('%s already loaded.'), module_name));
  end
end
%=============================================================================
