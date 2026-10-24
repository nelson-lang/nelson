%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_install_registry_dry_run(packageName, versionSpec)
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(versionSpec)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  packageInfo = nmm_registry('info', packageName, versionSpec);
  context = struct();
  context.plan = struct([]);
  context.conflicts = {};
  context.warnings = {};
  context.stack = {};
  context = addRegistryPackagePlan(context, packageInfo);
  report = struct();
  report.ok = isempty(context.conflicts);
  report.package = packageInfo.name;
  report.version = packageInfo.version;
  report.dependencies = packageDependencies(packageInfo);
  report.install_plan = context.plan;
  report.conflicts = context.conflicts;
  report.warnings = context.warnings;
end
%=============================================================================
function context = addRegistryPackagePlan(context, packageInfo)
  validateRegistryPackageCompatibility(packageInfo);
  source = packageSource(packageInfo);
  checksumVerified = verifyRegistryPackageSource(packageInfo, source);
  item = packagePlanItem(packageInfo, source, checksumVerified);
  if nmm_is_installed(packageInfo.name, packageInfo.version)
    item.action = 'already-installed';
    context = appendPlan(context, item);
    return
  end
  context = appendPlan(context, item);
  dependencies = packageDependencies(packageInfo);
  validateRegistryDependencies(dependencies);
  for dependency = fieldnames(dependencies)'
    dependencyName = dependency{1};
    dependencySpec = dependencies.(dependencyName);
    context = addDependencyPlan(context, dependencyName, dependencySpec);
  end
end
%=============================================================================
function context = addDependencyPlan(context, dependencyName, dependencySpec)
  if dependencyIsInstalled(dependencyName, dependencySpec)
    context = appendPlan(context, installedPlanItem(dependencyName, dependencySpec));
    return
  end
  if isDependencySource(dependencySpec)
    context = appendPlan(context, sourceDependencyPlanItem(dependencyName, dependencySpec));
    return
  end
  packageInfo = nmm_registry('info', dependencyName, dependencySpec);
  if any(strcmp(context.stack, dependencyName))
    nmm_error('DependencyCycle', 'Dependency cycle detected: %s', dependencyName);
  end
  context.stack{end + 1} = dependencyName;
  context = addRegistryPackagePlan(context, packageInfo);
  context.stack(end) = [];
end
%=============================================================================
function context = appendPlan(context, item)
  for k = 1:length(context.plan)
    if strcmp(context.plan(k).package, item.package)
      if ~strcmp(context.plan(k).version, item.version)
        context.conflicts{end + 1} = sprintf('%s: %s vs %s', item.package, context.plan(k).version, item.version);
      end
      return
    end
  end
  if isempty(context.plan)
    context.plan = item;
  else
    context.plan(end + 1) = item;
  end
end
%=============================================================================
function item = packagePlanItem(packageInfo, source, checksumVerified)
  item = struct();
  item.package = packageInfo.name;
  item.version = packageInfo.version;
  item.action = 'install';
  item.source = source;
  item.source_available = sourceAvailable(source);
  item.checksum = packageChecksum(packageInfo);
  item.checksum_verified = checksumVerified;
  item.dependencies = packageDependencies(packageInfo);
  item.requested = '';
end
%=============================================================================
function item = installedPlanItem(dependencyName, dependencySpec)
  [~, moduleData] = nmm_find_installed_module(dependencyName);
  item = emptyPlanItem(dependencyName, moduleData.version);
  item.action = 'already-satisfied';
  item.source = '';
  item.source_available = true;
  item.dependencies = struct();
  item.requested = dependencySpec;
end
%=============================================================================
function item = sourceDependencyPlanItem(dependencyName, dependencySpec)
  item = emptyPlanItem(dependencyName, dependencySpec);
  item.action = 'install-source';
  item.source = dependencySpec;
  item.source_available = sourceAvailable(dependencySpec);
  item.dependencies = struct();
  item.requested = dependencySpec;
end
%=============================================================================
function item = emptyPlanItem(packageName, packageVersion)
  item = struct();
  item.package = packageName;
  item.version = packageVersion;
  item.action = '';
  item.source = '';
  item.source_available = false;
  item.checksum = '';
  item.checksum_verified = false;
  item.dependencies = struct();
  item.requested = '';
end
%=============================================================================
function dependencies = packageDependencies(packageInfo)
  dependencies = struct();
  if isfield(packageInfo, 'dependencies')
    dependencies = packageInfo.dependencies;
  end
end
%=============================================================================
function validateRegistryDependencies(dependencies)
  if ~isstruct(dependencies)
    nmm_error('RegistryDependenciesInvalid', 'Package registry dependencies must be a struct.');
  end
  for dependency = fieldnames(dependencies)'
    if ~ischar(dependencies.(dependency{1}))
      nmm_error('InvalidDependencyDeclaration', 'Invalid dependency declaration: %s', dependency{1});
    end
  end
end
%=============================================================================
function tf = dependencyIsInstalled(dependencyName, dependencySpec)
  tf = false;
  if ~nmm_is_installed(dependencyName)
    return
  end
  if isDependencySource(dependencySpec)
    tf = true;
    return
  end
  modules = nmm_list();
  dependencyData = getfield(modules, dependencyName);
  versions = installedVersions(dependencyData);
  for k = 1:length(versions)
    if versionSatisfies(versions(k).version, dependencySpec)
      tf = true;
      return
    end
  end
  installed = installedVersionsText(versions);
  nmm_error('DependencyConflictRequiresInstalledVersions', '%s requires %s, installed versions: %s', dependencyName, dependencySpec, installed);
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
function text = installedVersionsText(versions)
  values = {};
  for k = 1:length(versions)
    values{end + 1} = versions(k).version;
  end
  text = strjoin(values, ', ');
end
%=============================================================================
function validateRegistryPackageCompatibility(packageInfo)
  if isfield(packageInfo, 'platforms')
    [r, msg] = nmm_is_supported_platform(packageInfo.platforms);
    if ~r
      nmm_error('PlatformNotSupported', msg);
    end
  end
  verifyArtifactForArch(packageInfo);
  if isfield(packageInfo, 'nelson') && ~nelsonVersionSatisfies(packageInfo.nelson)
    nmm_error('NelsonVersionNotSupported', 'Nelson version not supported: %s', packageInfo.nelson);
  end
end
%=============================================================================
function verifyArtifactForArch(packageInfo)
  % A binary package ships one archive per architecture under "artifacts": when
  % the running architecture has no archive, fail with a clear message rather
  % than a later missing-source error.
  if ~isfield(packageInfo, 'artifacts') || ~isstruct(packageInfo.artifacts) ...
      || isempty(fieldnames(packageInfo.artifacts))
    return
  end
  arch = computer('arch');
  if ~isfield(packageInfo.artifacts, arch)
    nmm_error('ArchiveNotAvailable', 'No %s archive available for %s %s.', arch, packageInfo.name, packageInfo.version);
  end
end
%=============================================================================
function tf = nelsonVersionSatisfies(range)
  if ~ischar(range)
    tf = false;
    return
  end
  v = version('-number');
  current = sprintf('%d.%d.%d', v(1), v(2), v(3));
  if isVersionConstraint(range)
    tf = semverConstraintSatisfied(current, range);
  else
    tf = semver(current, range) == 0;
  end
end
%=============================================================================
function checksumVerified = verifyRegistryPackageSource(packageInfo, source)
  checksumVerified = false;
  if ~sourceAvailable(source)
    nmm_error('RegistryPackageSourceMissingPackageRegistryEntry', 'Package registry entry source does not exist.');
  end
  if ~isfile(source)
    return
  end
  checksum = packageChecksum(packageInfo);
  if isempty(checksum)
    return
  end
  if ~strcmpi(checksum, sha256(source, '-file'))
    nmm_error('RegistryPackageChecksumMismatch', 'Registry checksum does not match package source.');
  end
  checksumVerified = true;
end
%=============================================================================
function checksum = packageChecksum(packageInfo)
  checksum = '';
  if isfield(packageInfo, 'checksum') && ischar(packageInfo.checksum)
    checksum = packageInfo.checksum;
  end
end
%=============================================================================
function source = packageSource(packageInfo)
  source = '';
  if isfield(packageInfo, 'source')
    source = packageInfo.source;
  elseif isfield(packageInfo, 'url')
    source = packageInfo.url;
  elseif isfield(packageInfo, 'archive')
    source = packageInfo.archive;
  end
  if ~ischar(source) || isempty(source)
    nmm_error('RegistryPackageSourceMissing', 'Package registry entry has no install source.');
  end
end
%=============================================================================
function tf = sourceAvailable(source)
  tf = isfile(source) || isdir(source) || isHttpSource(source);
end
%=============================================================================
function tf = isHttpSource(source)
  tf = ischar(source) && (startsWith(source, 'http://') || startsWith(source, 'https://'));
end
%=============================================================================
function tf = isDependencySource(dependencySpec)
  [isRepository, url, tag_branch] = nmm_is_http_repository(dependencySpec);
  tf = isRepository || isdir(dependencySpec) || isfile(dependencySpec);
end
%=============================================================================
function tf = versionSatisfies(installedVersion, dependencySpec)
  if isVersionConstraint(dependencySpec)
    tf = semverConstraintSatisfied(installedVersion, dependencySpec);
  else
    tf = semver(installedVersion, dependencySpec) == 0;
  end
end
%=============================================================================
function tf = isVersionConstraint(value)
  tf = startsWith(value, '>') || startsWith(value, '<') ...
      || startsWith(value, '=') || startsWith(value, '^') ...
      || startsWith(value, '~') || contains(value, ' ');
end
%=============================================================================
function tf = semverConstraintSatisfied(version, constraint)
  tf = true;
  constraints = strsplit(strtrim(constraint), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    try
      if semver(version, item) ~= 1
        tf = false;
        return
      end
    catch
      tf = false;
      return
    end
  end
end
%=============================================================================
