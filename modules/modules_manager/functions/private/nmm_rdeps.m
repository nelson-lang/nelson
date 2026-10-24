%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_rdeps(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = varargin{1};
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  report = struct([]);
  modules = nmm_list();
  for moduleName = fieldnames(modules)'
    requester = moduleName{1};
    versions = installedVersions(modules.(requester));
    for k = 1:length(versions)
      dependencies = installedDependencies(versions(k));
      if isfield(dependencies, packageName)
        report = appendReverseDependency(report, requester, versions(k), packageName, dependencies.(packageName));
      end
    end
  end
end
%=============================================================================
function report = appendReverseDependency(report, requester, versionData, dependencyName, dependencyVersion)
  item = struct();
  item.package = requester;
  item.version = versionData.version;
  item.path = versionData.path;
  item.dependency = dependencyName;
  item.requested = dependencyVersion;
  if isempty(report)
    report = item;
  else
    report(end + 1) = item;
  end
end
%=============================================================================
function dependencies = installedDependencies(versionData)
  dependencies = struct();
  if ~isfield(versionData, 'path')
    return
  end
  lockFile = [versionData.path, 'module-lock.json'];
  if ~isfile(lockFile)
    return
  end
  try
    lock = jsondecode(lockFile, '-file');
    if isfield(lock, 'dependencies') && isstruct(lock.dependencies)
      dependencies = lock.dependencies;
    end
  catch
  end
end
%=============================================================================
function versions = installedVersions(moduleData)
  versions = struct([]);
  if isfield(moduleData, 'versions') && ~isempty(moduleData.versions)
    versions = moduleData.versions;
    return
  end
  if isfield(moduleData, 'path') && isfield(moduleData, 'version')
    versions = moduleData;
    if isfield(versions, 'versions')
      versions = rmfield(versions, 'versions');
    end
    if isfield(versions, 'pinned_version')
      versions = rmfield(versions, 'pinned_version');
    end
  end
end
%=============================================================================
