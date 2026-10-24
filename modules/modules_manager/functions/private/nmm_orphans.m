%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_orphans(varargin)
  jsonOutput = parseOptions(varargin);
  report = orphanPackages();
  if jsonOutput
    report = jsonprettyprint(jsonencode(report));
  end
end
%=============================================================================
function jsonOutput = parseOptions(args)
  jsonOutput = false;
  if length(args) > 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if length(args) == 1
    if ~ischar(args{1})
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
    end
    if ~strcmp(args{1}, '-json')
      nmm_error('InvalidOption', '#%d argument: %s expected.', 2, '''-json''');
    end
    jsonOutput = true;
  end
end
%=============================================================================
function report = orphanPackages()
  modules = nmm_list();
  reachable = reachableDependencies(modules);
  report = struct([]);
  for package = fieldnames(modules)'
    packageName = package{1};
    if isAutoInstalledPackage(modules.(packageName)) && ~hasTextValue(reachable, packageName)
      report = appendOrphan(report, packageName, modules.(packageName));
    end
  end
end
%=============================================================================
function reachable = reachableDependencies(modules)
  reachable = {};
  for package = fieldnames(modules)'
    packageName = package{1};
    if ~isAutoInstalledPackage(modules.(packageName))
      reachable = addDependencies(reachable, modules, packageName);
    end
  end
end
%=============================================================================
function reachable = addDependencies(reachable, modules, packageName)
  if ~isfield(modules, packageName)
    return
  end
  dependencies = installedDependencies(modules.(packageName));
  for dependency = fieldnames(dependencies)'
    dependencyName = dependency{1};
    if hasTextValue(reachable, dependencyName)
      continue
    end
    reachable{end + 1} = dependencyName;
    reachable = addDependencies(reachable, modules, dependencyName);
  end
end
%=============================================================================
function report = appendOrphan(report, packageName, moduleData)
  item = struct();
  item.package = packageName;
  item.version = moduleData.version;
  item.path = moduleData.path;
  item.reason = 'auto-installed and no longer required';
  item.required_by = nmm_rdeps(packageName);
  if isempty(report)
    report = item;
  else
    report(end + 1) = item;
  end
end
%=============================================================================
function dependencies = installedDependencies(moduleData)
  dependencies = struct();
  if ~isfield(moduleData, 'path')
    return
  end
  lockFile = [moduleData.path, 'module-lock.json'];
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
function tf = isAutoInstalledPackage(moduleData)
  tf = isAutoInstalled(moduleData);
  if isfield(moduleData, 'versions') && ~isempty(moduleData.versions)
    versions = moduleData.versions;
    tf = true;
    for k = 1:length(versions)
      tf = tf && isAutoInstalled(versions(k));
    end
  end
end
%=============================================================================
function tf = isAutoInstalled(moduleData)
  tf = false;
  if isfield(moduleData, 'installed_as_dependency')
    tf = moduleData.installed_as_dependency;
  elseif isfield(moduleData, 'auto_installed')
    tf = moduleData.auto_installed;
  end
end
%=============================================================================
function tf = hasTextValue(values, expected)
  tf = false;
  for k = 1:length(values)
    if strcmp(values{k}, expected)
      tf = true;
      return
    end
  end
end
%=============================================================================
