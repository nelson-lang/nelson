%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_load(varargin)
  nbArguments = length(varargin);
  if nbArguments < 1 || nbArguments > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if nbArguments == 1
    [module_name, module_data] = nmm_find_installed_module(varargin{1});
  else
    if ~ischar(varargin{2})
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
    end
    [module_name, module_data] = nmm_find_installed_module(varargin{1}, varargin{2});
  end

  if isempty(module_data)
    nmm_error('ModuleNameExpected', 'module name or existing directory installed is expected.');
  end
  if ~isfile([module_data.path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  if ~isfile([module_data.path, 'module.json'])
    nmm_error('ModuleJsonMissing', 'module.json is missing.');
  end
  if ismodule(module_name)
    if nbArguments == 1
      current = currentInstalledModule(module_name);
      if ~isempty(current) && isfield(current, 'path') && strcmp(current.path, module_data.path)
        warning('Nelson:module:alreadyLoaded', '%s already loaded.', module_name);
        return
      end
    end
    removemodule(module_name);
  end
  if nbArguments == 2
    activateInstalledVersion(module_name, module_data);
  end
  removeInstalledFunctionPaths(module_name);
  clear functions
  if ~ismodule(module_name)
    run([module_data.path, 'loader.m']);
  end
end
%=============================================================================
function module_data = currentInstalledModule(module_name)
  module_data = struct([]);
  modules = nmm_list();
  if isfield(modules, module_name)
    module_data = getfield(modules, module_name);
  end
end
%=============================================================================
function activateInstalledVersion(module_name, module_data)
  modules = nmm_list();
  if ~isfield(modules, module_name)
    return
  end
  current = getfield(modules, module_name);
  current.path = module_data.path;
  current.version = module_data.version;
  current.load = true;
  modules.(module_name) = current;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
end
%=============================================================================
function removeInstalledFunctionPaths(module_name)
  modules = nmm_list();
  if ~isfield(modules, module_name)
    return
  end
  module_data = getfield(modules, module_name);
  versions = installedVersions(module_data);
  currentPath = path();
  for k = 1:length(versions)
    functionsPath = [versions(k).path, 'functions'];
    if isdir(functionsPath) && contains(currentPath, functionsPath)
      rmpath(functionsPath);
    end
  end
end
%=============================================================================
function versions = installedVersions(module_data)
  versions = struct([]);
  if isfield(module_data, 'versions') && ~isempty(module_data.versions)
    versions = module_data.versions;
    return
  end
  if isfield(module_data, 'path') && isfield(module_data, 'version')
    versions = module_data;
    if isfield(versions, 'versions')
      versions = rmfield(versions, 'versions');
    end
    if isfield(versions, 'pinned_version')
      versions = rmfield(versions, 'pinned_version');
    end
  end
end
%=============================================================================
