%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_audit(varargin)
  if length(varargin) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  asJson = false;
  packageName = '';
  for k = 1:length(varargin)
    if ~ischar(varargin{k})
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 1);
    end
    if strcmp(varargin{k}, '-json')
      asJson = true;
    elseif isempty(packageName)
      packageName = varargin{k};
    else
      nmm_error('InvalidOption', '#%d argument: %s expected.', k + 1, '''-json''');
    end
  end
  if isempty(packageName)
    issues = auditAllInstalled();
  else
    issues = auditOneInstalled(packageName);
  end
  report = struct();
  report.ok = isempty(issues);
  report.issues = issues;
  if ~isempty(packageName)
    report.package = packageName;
  end
  if asJson
    report = jsonprettyprint(jsonencode(report));
  end
end
%=============================================================================
function issues = auditAllInstalled()
  issues = {};
  modules = nmm_list();
  for moduleName = fieldnames(modules)'
    name = moduleName{1};
    data = getfield(modules, name);
    issues = auditInstalledEntry(name, data, issues);
  end
end
%=============================================================================
function issues = auditOneInstalled(moduleName)
  issues = {};
  modules = nmm_list();
  if ~isfield(modules, moduleName)
    issues = addIssue(issues, moduleName, 'package is not installed');
    return
  end
  issues = auditInstalledEntry(moduleName, modules.(moduleName), issues);
end
%=============================================================================
function issues = auditInstalledEntry(moduleName, data, issues)
  versions = installedVersions(data);
  if isempty(versions)
    issues = addIssue(issues, moduleName, 'no installed version in modules.json');
    return
  end
  for k = 1:length(versions)
    issues = auditInstalledVersion(moduleName, versions(k), issues);
  end
end
%=============================================================================
function issues = auditInstalledVersion(moduleName, data, issues)
  label = [moduleName, '@', data.version];
  if ~isfield(data, 'path')
    issues = addIssue(issues, label, 'missing path in modules.json');
    return
  end
  if ~isdir(data.path)
    issues = addIssue(issues, label, 'installed path does not exist');
    return
  end
  moduleJson = [data.path, 'module.json'];
  if ~isfile(moduleJson)
    issues = addIssue(issues, label, 'module.json is missing');
  else
    try
      moduleInfo = jsondecode(moduleJson, '-file');
      if ~isfield(moduleInfo, 'module') || ~strcmp(moduleInfo.module, moduleName)
        issues = addIssue(issues, label, 'module.json module name mismatch');
      end
    catch
      issues = addIssue(issues, label, 'module.json cannot be parsed');
    end
  end
  lockJson = [data.path, 'module-lock.json'];
  if ~isfile(lockJson)
    issues = addIssue(issues, label, 'module-lock.json is missing');
  else
    issues = auditLockFile(moduleName, lockJson, issues, label);
  end
end
%=============================================================================
function issues = auditLockFile(moduleName, lockJson, issues, label)
  try
    lock = jsondecode(lockJson, '-file');
  catch
    issues = addIssue(issues, label, 'module-lock.json cannot be parsed');
    return
  end
  if ~isfield(lock, 'module') || ~strcmp(lock.module, moduleName)
    issues = addIssue(issues, label, 'module-lock.json module name mismatch');
  end
  required = {'version', 'platform', 'builtin', 'package_type', 'dependencies', 'checksums', 'nelson'};
  for field = required
    if ~isfield(lock, field{1})
      issues = addIssue(issues, label, ['module-lock.json field is missing: ', field{1}]);
    end
  end
  if isfield(lock, 'platform')
    if ~(strcmp(lock.platform, 'all') || strcmp(lock.platform, computer('arch')))
      issues = addIssue(issues, label, 'module-lock.json platform does not match current architecture');
    end
  end
  if isfield(lock, 'builtin') && isfield(lock, 'package_type')
    if ~strcmp(lock.package_type, packageType(lock.builtin))
      issues = addIssue(issues, label, 'module-lock.json package type does not match builtin flag');
    end
  end
  if isfield(lock, 'nelson')
    issues = auditNelsonLock(lock, issues, label);
  end
  if isfield(lock, 'dependencies')
    modules = nmm_list();
    for dependency = fieldnames(lock.dependencies)'
      dependencyName = dependency{1};
      dependencyVersion = lock.dependencies.(dependencyName);
      if ~isfield(modules, dependencyName)
        issues = addIssue(issues, label, ['dependency is not installed: ', dependencyName]);
      else
        dependencyData = getfield(modules, dependencyName);
        if ~hasInstalledVersion(installedVersions(dependencyData), dependencyVersion)
          issues = addIssue(issues, label, ['dependency version is not installed: ', dependencyName]);
        end
      end
    end
  end
  if isfield(lock, 'checksums')
    issues = auditChecksums(fileparts(lockJson), lock.checksums, issues, label);
  end
end
%=============================================================================
function issues = auditNelsonLock(lock, issues, label)
  for field = {'version', 'arch', 'abi'}
    if ~isfield(lock.nelson, field{1})
      issues = addIssue(issues, label, ['module-lock.json nelson field is missing: ', field{1}]);
    end
  end
  if isfield(lock, 'builtin') && lock.builtin
    if isfield(lock.nelson, 'arch') && ~strcmp(lock.nelson.arch, computer('arch'))
      issues = addIssue(issues, label, 'module-lock.json Nelson architecture does not match current architecture');
    end
    if isfield(lock.nelson, 'abi') && ~strcmp(lock.nelson.abi, nelsonAbiTag())
      issues = addIssue(issues, label, 'module-lock.json Nelson ABI does not match current ABI');
    end
  end
end
%=============================================================================
function type = packageType(isBuiltin)
  if isBuiltin
    type = 'binary';
  else
    type = 'source';
  end
end
%=============================================================================
function issues = auditChecksums(modulePath, checksums, issues, label)
  required = {'module_json', 'loader'};
  for field = required
    if ~isfield(checksums, field{1})
      issues = addIssue(issues, label, ['module-lock.json checksum is missing: ', field{1}]);
    end
  end
  for field = fieldnames(checksums)'
    name = field{1};
    filename = checksumFilename(modulePath, name);
    if isempty(filename)
      issues = addIssue(issues, label, ['module-lock.json checksum has unknown file: ', name]);
    elseif ~isfile(filename)
      issues = addIssue(issues, label, ['module-lock.json checksum file is missing: ', name]);
    elseif ~strcmpi(checksums.(name), sha256(filename, '-file'))
      issues = addIssue(issues, label, ['module-lock.json checksum mismatch: ', name]);
    end
  end
end
%=============================================================================
function filename = checksumFilename(modulePath, name)
  root = ensureTrailingSlash(modulePath);
  switch name
    case 'module_json'
      filename = [root, 'module.json'];
    case 'loader'
      filename = [root, 'loader.m'];
    case 'builder'
      filename = [root, 'builder.m'];
    case 'etc_startup'
      filename = [root, 'etc/startup.m'];
    case 'etc_finish'
      filename = [root, 'etc/finish.m'];
    otherwise
      filename = '';
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
function issues = addIssue(issues, moduleName, message)
  issues{end + 1} = [moduleName, ': ', message];
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
function tag = nelsonAbiTag()
  compiler = version('-compiler');
  tag = [computer('arch'), '-', compiler{3}];
end
%=============================================================================
