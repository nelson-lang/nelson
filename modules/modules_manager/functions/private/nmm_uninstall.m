%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_uninstall(varargin)
  if length(varargin) < 1 || length(varargin) > 3
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_or_directory = varargin{1};
  if ~ischar(module_or_directory)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  options = parseOptions(varargin(2:end));
  version = options.version;
  [module_name, module_data] = nmm_find_installed_module(module_or_directory, version);
  if isempty(module_data)
    nmm_error('ModuleNameExpected', 'module name or existing directory installed is expected.');
  end
  removeAll = isempty(version) && ~isdir(module_or_directory);
  report = removalReport(module_name, module_data, removeAll, options);
  if options.dry_run
    return
  end
  if ~options.force && ~isempty(report.required_by)
    nmm_error('PackageRequired', requiredByMessage(module_name));
  end
  nmm_progress('remove', module_name, report.version, 'remove', 1, 2);
  if removeAll
    uninstallAllVersions(module_name);
  else
    uninstallOneVersion(module_name, module_data);
  end
  nmm_progress('remove', module_name, report.version, 'done', 2, 2);
  report.removed = true;
  report.changed = true;
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.version = '';
  options.dry_run = false;
  options.force = false;
  for k = 1:length(args)
    value = args{k};
    if ~ischar(value)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
    end
    switch value
      case '-dry-run'
        options.dry_run = true;
      case '-force'
        options.force = true;
      otherwise
        if ~isempty(options.version)
          nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
        end
        options.version = value;
    end
  end
end
%=============================================================================
function report = removalReport(module_name, module_data, removeAll, options)
  report = struct();
  report.package = module_name;
  report.version = '';
  if ~removeAll
    report.version = module_data.version;
  end
  report.remove_all = removeAll;
  report.dry_run = options.dry_run;
  report.forced = options.force;
  report.removed = false;
  report.changed = false;
  report.paths = removalPaths(module_name, module_data, removeAll);
  report.required_by = reverseDependencies(module_name, report.version);
  report.allowed = options.force || isempty(report.required_by);
end
%=============================================================================
function paths = removalPaths(module_name, module_data, removeAll)
  if removeAll
    modules = nmm_list();
    versions = installedVersions(getfield(modules, module_name));
  else
    versions = module_data;
  end
  paths = {};
  for k = 1:length(versions)
    paths{end + 1} = versions(k).path;
  end
end
%=============================================================================
function report = reverseDependencies(module_name, version)
  all = nmm_rdeps(module_name);
  report = struct([]);
  for k = 1:length(all)
    if isempty(version) || strcmp(all(k).requested, version)
      report = appendReverseDependency(report, all(k));
    end
  end
end
%=============================================================================
function report = appendReverseDependency(report, item)
  if isempty(report)
    report = item;
  else
    report(end + 1) = item;
  end
end
%=============================================================================
function msg = requiredByMessage(module_name)
  msg = sprintf(_('Package is required by installed modules: %s. Use -force to remove it.'), module_name);
end
%=============================================================================
function uninstallAllVersions(module_name)
  modules = nmm_list();
  module_record = getfield(modules, module_name);
  versions = installedVersions(module_record);
  if ismodule(module_name)
    removemodule(module_name);
  end
  if ismodule('help_tools')
    deployhelp('remove', module_name);
  end
  for k = 1:length(versions)
    removeInstalledDirectory(versions(k).path);
  end
  modules = rmfield(modules, module_name);
  writeModulesJson(modules);
end
%=============================================================================
function uninstallOneVersion(module_name, module_data)
  modules = nmm_list();
  module_record = getfield(modules, module_name);
  activePath = module_record.path;
  if ismodule(module_name) && strcmp(activePath, module_data.path)
    removemodule(module_name);
  end
  if ismodule('help_tools')
    deployhelp('remove', module_name);
  end
  removeInstalledDirectory(module_data.path);
  versions = removeInstalledVersion(installedVersions(module_record), module_data.version);
  if isempty(versions)
    modules = rmfield(modules, module_name);
  else
    active = selectActiveAfterRemove(module_record, versions);
    modules.(module_name) = active;
  end
  writeModulesJson(modules);
end
%=============================================================================
function removeInstalledDirectory(module_path)
  p = usermodulesdir();
  if ~startsWith(module_path, p)
    return
  end
  if ~isdir(module_path)
    return
  end
  parent_dir = fullpath([module_path, '/..']);
  d = dir(parent_dir);
  if length(d) == 3
    [r, msg] = rmdir(parent_dir, 's');
  else
    [r, msg] = rmdir(module_path, 's');
  end
  if ~r
    nmm_error('InstalledDirectoryRemoveFailed', msg);
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
function versions = removeInstalledVersion(versions, version)
  keep = [];
  for k = 1:length(versions)
    if ~strcmp(versions(k).version, version)
      keep = [keep, k];
    end
  end
  versions = versions(keep);
end
%=============================================================================
function active = selectActiveAfterRemove(module_record, versions)
  activeVersion = latestInstalledVersion(versions);
  if isfield(module_record, 'pinned_version') && hasInstalledVersion(versions, module_record.pinned_version)
    activeVersion = module_record.pinned_version;
  end
  active = selectInstalledVersion(versions, activeVersion);
  active.versions = versions;
  if isfield(module_record, 'pinned_version') && hasInstalledVersion(versions, module_record.pinned_version)
    active.pinned_version = module_record.pinned_version;
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
  selected = struct([]);
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      selected = versions(k);
      return
    end
  end
end
%=============================================================================
function writeModulesJson(modules)
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
end
%=============================================================================
