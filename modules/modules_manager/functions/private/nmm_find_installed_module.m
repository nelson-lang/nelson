%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [module_name, module_data] = nmm_find_installed_module(module_or_path, varargin)
  module_name = [];
  module_data = struct([]);
  version = '';
  nbArguments = 1 + length(varargin);
  if nbArguments == 2
    version = varargin{1};
  elseif nbArguments ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end

  modules = nmm_list();
  if isempty(modules)
    return
  end
  if isfield(modules, module_or_path)
    module_name = module_or_path;
    module_data = selectInstalledVersion(getfield(modules, module_or_path), version);
    return
  end
  if ~isempty(version) || ~isdir(module_or_path)
    return
  end
  module_or_path = fullpath(module_or_path);
  if ~endsWith(module_or_path, '/')
    module_or_path = [module_or_path, '/'];
  end
  for m = fieldnames(modules)'
    selected = selectInstalledPath(getfield(modules, m{1}), module_or_path);
    if ~isempty(selected)
      module_name = m{1};
      module_data = selected;
      return
    end
  end
end
%=============================================================================
function selected = selectInstalledVersion(module_data, version)
  selected = struct([]);
  versions = installedVersions(module_data);
  if isempty(version)
    if isfield(module_data, 'path') && isfield(module_data, 'version')
      selected.path = module_data.path;
      selected.load = module_data.load;
      selected.version = module_data.version;
    elseif ~isempty(versions)
      selected = versions(1);
    end
    return
  end
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      selected = versions(k);
      return
    end
  end
end
%=============================================================================
function selected = selectInstalledPath(module_data, module_path)
  selected = struct([]);
  versions = installedVersions(module_data);
  for k = 1:length(versions)
    if strcmp(versions(k).path, module_path)
      selected = versions(k);
      return
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
