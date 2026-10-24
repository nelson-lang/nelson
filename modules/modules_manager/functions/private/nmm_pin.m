%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_pin(varargin)
  if length(varargin) < 1 || length(varargin) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_name = varargin{1};
  version = '';
  if length(varargin) == 2
    version = varargin{2};
  end
  if ~ischar(module_name)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if length(varargin) == 2 && ~ischar(version)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  [module_name, module_data] = nmm_find_installed_module(module_name, version);
  if isempty(module_data)
    nmm_error('ModuleNameExpectedModuleNameInstalled', 'module name and installed version expected.');
  end
  if ismodule(module_name)
    removemodule(module_name);
  end
  modules = nmm_list();
  current = getfield(modules, module_name);
  current.path = module_data.path;
  current.version = module_data.version;
  current.load = true;
  current.pinned_version = module_data.version;
  modules.(module_name) = current;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
  nmm_load(module_name, module_data.version);
  report = struct();
  report.module = module_name;
  report.version = module_data.version;
  report.path = module_data.path;
end
%=============================================================================
