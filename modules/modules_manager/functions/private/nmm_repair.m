%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_repair(varargin)
  if length(varargin) ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  modules = nmm_list();
  removed = {};
  for moduleName = fieldnames(modules)'
    name = moduleName{1};
    data = getfield(modules, name);
    [data, removed] = removeBrokenVersions(name, data, removed);
    if isempty(data)
      modules = rmfield(modules, name);
    else
      modules.(name) = data;
    end
  end
  saveModulesJson(modules);
  report = struct();
  report.removed = removed;
  report.ok = true;
end
%=============================================================================
function [data, removed] = removeBrokenVersions(moduleName, data, removed)
  versions = installedVersions(data);
  valid = struct([]);
  for k = 1:length(versions)
    if isfield(versions(k), 'path') && isdir(versions(k).path)
      valid = appendVersion(valid, versions(k));
    else
      removed{end + 1} = [moduleName, '@', versions(k).version];
    end
  end
  if isempty(valid)
    data = struct([]);
    if isempty(versions)
      removed{end + 1} = moduleName;
    end
    return
  end
  data = selectActiveVersion(data, valid);
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
function versions = appendVersion(versions, versionData)
  if isempty(versions)
    versions = versionData;
  else
    versions(end + 1) = versionData;
  end
end
%=============================================================================
function data = selectActiveVersion(previousData, versions)
  activeVersion = latestInstalledVersion(versions);
  if isfield(previousData, 'pinned_version') && hasInstalledVersion(versions, previousData.pinned_version)
    activeVersion = previousData.pinned_version;
  end
  data = selectInstalledVersion(versions, activeVersion);
  data.versions = versions;
  if isfield(previousData, 'pinned_version') && hasInstalledVersion(versions, previousData.pinned_version)
    data.pinned_version = previousData.pinned_version;
  end
end
%=============================================================================
function version = latestInstalledVersion(versions)
  version = versions(1).version;
  for k = 2:length(versions)
    if semver(versions(k).version, version) == 1
      version = versions(k).version;
    end
  end
end
%=============================================================================
function r = hasInstalledVersion(versions, version)
  r = false;
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      r = true;
      return
    end
  end
end
%=============================================================================
function selected = selectInstalledVersion(versions, version)
  selected = versions(1);
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      selected = versions(k);
      return
    end
  end
end
%=============================================================================
function saveModulesJson(modules)
  modules_json_path = [usermodulesdir(), 'modules.json'];
  txt = jsonprettyprint(jsonencode(modules));
  filewrite(modules_json_path, txt);
end
%=============================================================================
