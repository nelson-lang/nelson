%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_install(varargin)
  report = [];
  switch length(varargin)
    case 1
      nmm_install_one_rhs(varargin{1});
    case 2
      report = nmm_install_two_rhs(varargin{1}, varargin{2});
    case 3
      report = nmm_install_options(varargin{1}, varargin{2}, varargin{3});
    case 4
      report = nmm_install_options(varargin{1}, varargin{2}, varargin{3}, varargin{4});
    otherwise
      nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
% nmm('install', module_name)
% nmm('install', local_module_directory)
% nmm('install', http_git_repository)
function nmm_install_one_rhs(param1, varargin)
  if ~ischar(param1)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  runTests = testsFromEnvironment();
  if nargin == 2
    runTests = varargin{1};
  end
  if isfile(param1)
    nmm_install_file(param1);
  else
    if isdir(param1)
      nmm_install_directory(param1, runTests);
    else
      [res, url, tag_branch] = nmm_is_http_repository(param1);
      if res
        nmm_install_repository(url, tag_branch, runTests);
      else
        nmm_error('SourceExpected', 'An valid git url or local directory expected.');
      end
    end
  end
end
%=============================================================================
function report = nmm_install_two_rhs(module_name, module_version)
  report = [];
  if ~ischar(module_name)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(module_version)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if strcmp(module_version, '-dry-run')
    if isfile(module_name)
      report = dryRunPackageInstall(module_name);
    else
      report = nmm_install_registry_dry_run(module_name, '');
    end
  elseif strcmp(module_version, '-force')
    if ~isfile(module_name)
      nmm_error('PackageFilenameExpected', '#%d argument: package filename expected.', 2);
    end
    report = nmm_install_force_package(module_name);
  elseif strcmp(module_version, '-no-tests') || strcmp(module_version, '-tests')
    installSourceWithTests(module_name, strcmp(module_version, '-tests'));
  elseif strcmp(module_version, '-offline')
    installOfflinePackage(module_name, '');
  else
    packageInfo = nmm_registry('info', module_name, module_version);
    installPackageInfo(packageInfo);
  end
end
%=============================================================================
function report = dryRunPackageInstall(filename)
  if ~endsWith(filename, '.nmz')
    nmm_error('InvalidPackageFileExtension', 'filename extension .nmz expected.');
  end
  verifyPackageChecksum(filename);
  stagingPath = newTemporaryDirectory();
  try
    unzip(filename, stagingPath);
    nmm_validate(stagingPath);
    validateBuiltPackageStructure(stagingPath);
    lockFile = [stagingPath, 'module-lock.json'];
    if ~isfile(lockFile)
      nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
    end
    info = jsondecode(lockFile, '-file');
    moduleInfo = nmm_read_module_json(stagingPath);
    validateArchiveLock(info, moduleInfo);
    validateLockChecksums(stagingPath, info);
    checkInstalledDependencies(info);
    destination = [usermodulesdir(), info.module, '/', info.version, '/'];
    report = struct();
    report.ok = true;
    report.module = info.module;
    report.version = info.version;
    report.destination = destination;
    report.destination_exists = isdir(destination);
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function installOfflinePackage(module_name, versionSpec)
  source = nmm_cache('latest', module_name, versionSpec);
  nmm_install_one_rhs(source);
end
%=============================================================================
function installPackageInfo(packageInfo)
  refuseYankedPackage(packageInfo);
  verifyRegistryPackageCompatibility(packageInfo);
  installRegistryDependencies(packageInfo);
  source = packageSource(packageInfo);
  if isHttpArchiveSource(source)
    installRemoteArchive(source, packageInfo);
    return
  end
  verifyRegistryChecksum(packageInfo, source);
  nmm_install_one_rhs(source, testsPolicyForPackage(packageInfo));
end
%=============================================================================
function tf = isHttpArchiveSource(source)
  tf = ischar(source) && endsWith(lower(source), '.nmz') ...
      && (startsWith(source, 'http://') || startsWith(source, 'https://'));
end
%=============================================================================
function installRemoteArchive(source, packageInfo)
  % Download a hosted .nmz to a temporary file, verify its checksum against
  % the registry entry (mandatory for a remote archive), then install it.
  checksum = '';
  if isfield(packageInfo, 'checksum') && ischar(packageInfo.checksum)
    checksum = packageInfo.checksum;
  end
  if isempty(checksum)
    nmm_error('RegistryPackageChecksumRequired', 'A hosted .nmz package must declare a checksum in the registry.');
  end
  tempDir = newTemporaryDirectory();
  target = [tempDir, packageInfo.name, '-', packageInfo.version, '.nmz'];
  try
    websave(target, source, nmm_web_options());
  catch
    e = lasterror();
    cleanupDirectory(tempDir);
    nmm_error('RegistryPackageDownloadFailed', 'Cannot download package archive: %s (%s)', source, e.message);
  end
  if ~strcmpi(checksum, sha256(target, '-file'))
    cleanupDirectory(tempDir);
    nmm_error('RegistryPackageChecksumMismatch', 'Registry checksum does not match package source.');
  end
  try
    nmm_install_file(target);
    cleanupDirectory(tempDir);
  catch
    e = lasterror();
    cleanupDirectory(tempDir);
    error(e);
  end
end
%=============================================================================
function installSourceWithTests(source, runTests)
  if isdir(source)
    nmm_install_directory(source, runTests);
    return
  end
  [isRepository, url, tag_branch] = nmm_is_http_repository(source);
  if isRepository
    nmm_install_repository(url, tag_branch, runTests);
    return
  end
  if isfile(source)
    % prebuilt archives were tested when packed: nothing to run here
    nmm_install_file(source);
    return
  end
  nmm_error('SourceExpected', 'An valid git url or local directory expected.');
end
%=============================================================================
function refuseYankedPackage(packageInfo)
  % A yanked version stays resolvable for lockfiles but is never installed anew.
  if ~isfield(packageInfo, 'yanked') || isempty(packageInfo.yanked)
    return
  end
  value = packageInfo.yanked;
  yanked = (islogical(value) || isnumeric(value)) && logical(value(1));
  yanked = yanked || (ischar(value) && any(strcmpi(value, {'true', '1', 'yes'})));
  if ~yanked
    return
  end
  reason = '';
  if isfield(packageInfo, 'yank_reason') && ischar(packageInfo.yank_reason) && ~isempty(packageInfo.yank_reason)
    reason = [': ', packageInfo.yank_reason];
  end
  nmm_error('PackageYanked', 'Package version has been yanked from the registry: %s %s%s', packageInfo.name, packageInfo.version, reason);
end
%=============================================================================
function runTests = testsFromEnvironment()
  % Package tests are opt-in at install time: NELSON_NMM_INSTALL_TESTS=1
  % (or true/on/yes) runs them for every source install.
  value = lower(strtrim(getenv('NELSON_NMM_INSTALL_TESTS')));
  runTests = any(strcmp(value, {'1', 'true', 'on', 'yes'}));
end
%=============================================================================
function runTests = testsPolicyForPackage(packageInfo)
  % A registry entry can require the package tests ("tests": "required").
  runTests = testsFromEnvironment();
  if isfield(packageInfo, 'tests') && ischar(packageInfo.tests) ...
      && strcmpi(packageInfo.tests, 'required')
    runTests = true;
  end
end
%=============================================================================
function steps = sourceSteps(runTests)
  % Progress plan of a source install: the test step only counts when it runs.
  steps = struct();
  steps.total = 5 + double(logical(runTests));
  steps.fetch = 1;
  steps.dependencies = 2;
  steps.build = 3;
  steps.test = 4;
  steps.register = steps.total - 1;
  steps.done = steps.total;
end
%=============================================================================
function installRegistryDependencies(packageInfo)
  if ~isfield(packageInfo, 'dependencies')
    return
  end
  if ~isstruct(packageInfo.dependencies)
    nmm_error('RegistryDependenciesInvalid', 'Package registry dependencies must be a struct.');
  end
  for dependency = fieldnames(packageInfo.dependencies)'
    dependencyName = dependency{1};
    dependencySpec = packageInfo.dependencies.(dependencyName);
    ensureInstalledDependency(packageInfo.name, dependencyName, dependencySpec);
  end
end
%=============================================================================
function verifyRegistryPackageCompatibility(packageInfo)
  if isfield(packageInfo, 'platforms')
    [r, msg] = nmm_is_supported_platform(packageInfo.platforms);
    if ~r
      nmm_error('PlatformNotSupported', msg);
    end
  end
  verifyArtifactForArch(packageInfo);
  if isfield(packageInfo, 'nelson')
    if ~nelsonVersionSatisfies(packageInfo.nelson)
      nmm_error('NelsonVersionNotSupported', 'Nelson version not supported: %s', packageInfo.nelson);
    end
  end
end
%=============================================================================
function verifyArtifactForArch(packageInfo)
  % A binary package ships one archive per architecture under "artifacts". When
  % the running architecture has no archive (for instance a version that only
  % published win64 so far), fail with a clear message instead of a later
  % missing-source error.
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
function r = nelsonVersionSatisfies(range)
  if ~ischar(range)
    r = false;
    return
  end
  v = version('-number');
  current = sprintf('%d.%d.%d', v(1), v(2), v(3));
  if isVersionConstraint(range)
    r = semverConstraintSatisfied(current, range);
  else
    r = semver(current, range) == 0;
  end
end
%=============================================================================
function tf = isVersionConstraint(value)
  tf = startsWith(value, '>') || startsWith(value, '<') ...
      || startsWith(value, '=') || startsWith(value, '^') ...
      || startsWith(value, '~') || contains(value, ' ');
end
%=============================================================================
function verifyRegistryChecksum(packageInfo, source)
  if ~isfile(source)
    return
  end
  if ~isfield(packageInfo, 'checksum')
    return
  end
  checksum = packageInfo.checksum;
  if ~ischar(checksum) || isempty(checksum)
    return
  end
  if ~strcmpi(checksum, sha256(source, '-file'))
    nmm_error('RegistryPackageChecksumMismatch', 'Registry checksum does not match package source.');
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
function nmm_install_file(filename)
  if ~endsWith(filename, '.nmz')
    nmm_error('InvalidPackageFileExtension', 'filename extension .nmz expected.');
  end
  verifyPackageChecksum(filename);
  stagingPath = newTemporaryDirectory();
  try
    unzip(filename, stagingPath);
    nmm_validate(stagingPath);
    validateBuiltPackageStructure(stagingPath);
    lockFile = [stagingPath, 'module-lock.json'];
    if ~isfile(lockFile)
      nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
    end
    info = jsondecode(lockFile, '-file');
    nmm_progress('install', info.module, info.version, 'extract', 1, 3);
    if nmm_is_installed(info.module, info.version)
      warning('Nelson:module:alreadyInstalled', '%s already installed.', info.module);
      cleanupDirectory(stagingPath);
      return
    end
    moduleInfo = nmm_read_module_json(stagingPath);
    validateArchiveLock(info, moduleInfo);
    validateLockChecksums(stagingPath, info);
    checkInstalledDependencies(info);
    isSupported = strcmp(info.platform, 'all') || strcmp(info.platform, computer('arch'));
    if ~isSupported
      nmm_error('PlatformNotSupported', 'platform not supported.');
    end
    nmm_progress('install', info.module, info.version, 'register', 2, 3);
    commitPreparedModule(stagingPath, info.module, info.version);
    nmm_cache('store', filename, info.module, info.version);
    nmm_progress('install', info.module, info.version, 'done', 3, 3);
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function validateArchiveLock(lock, moduleInfo)
  if ~isfield(lock, 'module') || ~strcmp(lock.module, moduleInfo.module)
    nmm_error('ModuleLockModuleMismatch', 'module-lock.json module name mismatch.');
  end
  if ~isfield(lock, 'version') || ~strcmp(lock.version, moduleInfo.version)
    nmm_error('ModuleLockVersionMismatch', 'module-lock.json module version mismatch.');
  end
  if ~isfield(lock, 'builtin') || lock.builtin ~= moduleInfo.builtin
    nmm_error('ModuleLockBuiltinMismatch', 'module-lock.json builtin mismatch.');
  end
  if ~isfield(lock, 'package_type') || ~strcmp(lock.package_type, packageType(moduleInfo))
    nmm_error('ModuleLockPackageTypeMismatch', 'module-lock.json package type mismatch.');
  end
  if ~isfield(lock, 'nelson') || ~isstruct(lock.nelson)
    nmm_error('ModuleLockNelsonMissing', 'module-lock.json field is missing: nelson');
  end
  if ~isfield(lock.nelson, 'version') || ~isfield(lock.nelson, 'arch') || ~isfield(lock.nelson, 'abi')
    nmm_error('ModuleLockNelsonMissing', 'module-lock.json field is missing: nelson');
  end
  if lock.builtin
    if ~strcmp(lock.nelson.arch, computer('arch'))
      nmm_error('NelsonArchitectureNotSupported', 'Nelson architecture not supported.');
    end
    if ~strcmp(lock.nelson.abi, nelsonAbiTag())
      nmm_error('NelsonAbiNotSupported', 'Nelson ABI not supported: %s', lock.nelson.abi);
    end
  end
end
%=============================================================================
function type = packageType(info)
  if info.builtin
    type = 'binary';
  else
    type = 'source';
  end
end
%=============================================================================
function verifyPackageChecksum(filename)
  checksumFile = [filename, '.sha256'];
  if ~isfile(checksumFile)
    return
  end
  expected = strtrim(fileread(checksumFile));
  separator = strfind(expected, ' ');
  if ~isempty(separator)
    expected = expected(1:separator(1) - 1);
  end
  actual = sha256(filename, '-file');
  if ~strcmpi(expected, actual)
    nmm_error('PackageChecksumMismatch', 'Package checksum does not match.');
  end
end
%=============================================================================
function validateLockChecksums(modulePath, lock)
  if ~isfield(lock, 'checksums') || ~isstruct(lock.checksums)
    nmm_error('ModuleLockChecksumsMissing', 'module-lock.json field is missing: checksums');
  end
  checksums = lock.checksums;
  required = {'module_json', 'loader'};
  for field = required
    if ~isfield(checksums, field{1})
      nmm_error('ModuleLockChecksumMissing', ['module-lock.json checksum is missing: ', field{1}]);
    end
  end
  for field = fieldnames(checksums)'
    name = field{1};
    filename = checksumFilename(modulePath, name);
    if isempty(filename)
      nmm_error('ModuleLockChecksumUnknownFile', ['module-lock.json checksum has unknown file: ', name]);
    end
    if ~isfile(filename)
      nmm_error('ModuleLockChecksumFileMissing', ['module-lock.json checksum file is missing: ', name]);
    end
    if ~strcmpi(checksums.(name), sha256(filename, '-file'))
      nmm_error('ModuleLockChecksumMismatch', ['module-lock.json checksum mismatch: ', name]);
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
function validateBuiltPackageStructure(module_path)
  if ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  if ~isfile([module_path, 'module-lock.json'])
    nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
  end
end
%=============================================================================
function nmm_install_directory(directory, varargin)
  runTests = testsFromEnvironment();
  if nargin == 2
    runTests = varargin{1};
  end
  steps = sourceSteps(runTests);
  directory = ensureTrailingSlash(directory);
  if ~is_local_module(directory)
    nmm_error('ModuleDirectoryExpectedValidModuleRepository', 'Valid module repository expected.');
  end
  nmm_validate(directory);
  module_description = nmm_read_module_json(directory);
  if nmm_is_installed(module_description.module, module_description.version)
    warning('Nelson:module:alreadyInstalled', '%s already installed.', module_description.module);
    return
  end
  nmm_progress('install', module_description.module, module_description.version, 'dependencies', steps.dependencies, steps.total);
  installModuleDependencies(directory, module_description);
  [r, msg] = nmm_is_supported_platform(module_description.platforms);
  if ~r
    nmm_error('PlatformNotSupported', msg);
  end
  if (module_description.builtin && ~havecompiler())
    installCompiler = false;
    if ispc()
      [installCompiler, message] = configuremsvc();
    end
    if installCompiler
      warning('Nelson:module:visualStudioConfigured', 'Visual studio detected and configured to be used with Nelson.');
    else
      nmm_error('CompilerRequired', 'An C/C++ compiler is required by %s', module_description.module);
    end
  end
  installSourceModule(directory, module_description, runTests);
end
%=============================================================================
function nmm_install_repository(url, tag_branch, varargin)
  runTests = testsFromEnvironment();
  if nargin == 3
    runTests = varargin{1};
  end
  steps = sourceSteps(runTests);
  sourcePath = newTemporaryDirectory();
  try
    repo('export', url, tag_branch, sourcePath);
    nmm_validate(sourcePath);
    module_description = nmm_read_module_json(sourcePath);
    nmm_progress('install', module_description.module, module_description.version, 'fetch', steps.fetch, steps.total);
    if nmm_is_installed(module_description.module, module_description.version)
      warning('Nelson:module:alreadyInstalled', '%s already installed.', module_description.module);
      cleanupDirectory(sourcePath);
      return
    end
    nmm_progress('install', module_description.module, module_description.version, 'dependencies', steps.dependencies, steps.total);
    installModuleDependencies(sourcePath, module_description);
    [r, msg] = nmm_is_supported_platform(module_description.platforms);
    if ~r
      nmm_error('PlatformNotSupported', msg);
    end
    if (module_description.builtin && ~havecompiler())
      installCompiler = false;
      if ispc()
        [installCompiler, message] = configuremsvc();
      end
      if installCompiler
        warning('Nelson:module:visualStudioConfigured', 'Visual studio detected and configured to be used with Nelson.');
      else
        nmm_error('CompilerRequired', 'An C/C++ compiler is required by %s', module_description.module);
      end
    end
    installSourceModule(sourcePath, module_description, runTests);
    cleanupDirectory(sourcePath);
  catch
    e = lasterror();
    cleanupDirectory(sourcePath);
    error(e);
  end
end
%=============================================================================
function r = is_local_module(param)
  r = false;
  if isdir(param)
    path = fullpath(param);
    if ~endsWith(path, '/')
      path = [path, '/'];
    end
    r = isfile([path, 'module.json']) && isfile([path, 'builder.m']);
  end
end
%=============================================================================
function installModuleDependencies(module_path, module_description)
  dependencies = resolvedSourceDependencies(module_path, module_description);
  if isempty(fieldnames(dependencies))
    return
  end
  for dependency = fieldnames(dependencies)'
    dependencyName = dependency{1};
    dependencySpec = dependencies.(dependencyName);
    ensureInstalledDependency(module_description.module, dependencyName, dependencySpec);
  end
end
%=============================================================================
function dependencies = resolvedSourceDependencies(module_path, module_description)
  if isSourceLocked(module_path)
    lock = jsondecode([ensureTrailingSlash(module_path), 'module-lock.json'], '-file');
    validateSourceLock(lock, module_description);
    dependencies = lock.dependencies;
    return
  end
  dependencies = struct();
  if isfield(module_description, 'dependencies')
    dependencies = module_description.dependencies;
  end
end
%=============================================================================
function r = isSourceLocked(module_path)
  r = isfile([ensureTrailingSlash(module_path), 'module-lock.json']);
end
%=============================================================================
function validateSourceLock(lock, module_description)
  if ~isfield(lock, 'module') || ~strcmp(lock.module, module_description.module)
    nmm_error('ModuleLockModuleMismatch', 'module-lock.json module name mismatch.');
  end
  if ~isfield(lock, 'version') || ~strcmp(lock.version, module_description.version)
    nmm_error('ModuleLockVersionMismatch', 'module-lock.json module version mismatch.');
  end
  if ~isfield(lock, 'dependencies') || ~isstruct(lock.dependencies)
    nmm_error('ModuleLockDependenciesMissing', 'module-lock.json field is missing: dependencies');
  end
end
%=============================================================================
function ensureInstalledDependency(requesterName, dependencyName, dependencySpec)
  if ~ischar(dependencySpec)
    nmm_error('InvalidDependencyDeclaration', 'Invalid dependency declaration: %s', dependencyName);
  end
  if nmm_is_installed(dependencyName)
    if isDependencySource(dependencySpec)
      return
    end
    modules = nmm_list();
    dependencyData = getfield(modules, dependencyName);
    if versionSatisfies(dependencyData.version, dependencySpec)
      return
    end
    compatibleVersion = findCompatibleInstalledVersion(dependencyData, dependencySpec);
    if ~isempty(compatibleVersion)
      nmm_load(dependencyName, compatibleVersion);
      return
    end
    versions = installedVersionsText(dependencyData);
    nmm_error('DependencyConflict', '%s requires %s %s, installed versions: %s', requesterName, dependencyName, dependencySpec, versions);
  end
  if isDependencySource(dependencySpec)
    nmm_install_one_rhs(dependencySpec);
  else
    try
      nmm_install_two_rhs(dependencyName, dependencySpec);
    catch
      e = lasterror();
      if isMissingRegistryDependencyError(e)
        nmm_error('DependencyNotInstalled', nmm_missing_dependency_message(dependencyName));
      end
      error(e);
    end
  end
  nmm_mark_installed_as_dependency(dependencyName);
end
%=============================================================================
function tf = isMissingRegistryDependencyError(e)
  tf = strcmp(e.identifier, 'Nelson:nmm:PackageNotFound') ...
      || strcmp(e.identifier, 'Nelson:nmm:RegistryNotConfigured');
end
%=============================================================================
function r = isDependencySource(dependencySpec)
  [isRepository, url, tag_branch] = nmm_is_http_repository(dependencySpec);
  r = isRepository || isdir(dependencySpec) || isfile(dependencySpec);
end
%=============================================================================
function r = versionSatisfies(installedVersion, dependencySpec)
  if startsWith(dependencySpec, '>') || startsWith(dependencySpec, '<') ...
      || startsWith(dependencySpec, '=') || startsWith(dependencySpec, '^') ...
      || startsWith(dependencySpec, '~') || contains(dependencySpec, ' ')
    r = semverConstraintSatisfied(installedVersion, dependencySpec);
  else
    r = semver(installedVersion, dependencySpec) == 0;
  end
end
%=============================================================================
function tf = semverConstraintSatisfied(installedVersion, dependencySpec)
  tf = true;
  constraints = strsplit(strtrim(dependencySpec), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    try
      if semver(installedVersion, item) ~= 1
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
function version = findCompatibleInstalledVersion(dependencyData, dependencySpec)
  version = '';
  versions = installedVersions(dependencyData);
  for k = 1:length(versions)
    if versionSatisfies(versions(k).version, dependencySpec)
      if isempty(version) || semver(versions(k).version, version) == 1
        version = versions(k).version;
      end
    end
  end
end
%=============================================================================
function text = installedVersionsText(dependencyData)
  versions = installedVersions(dependencyData);
  values = {};
  for k = 1:length(versions)
    values{end + 1} = versions(k).version;
  end
  text = strjoin(values, ', ');
  if isempty(text)
    text = _('none');
  end
end
%=============================================================================
function installSourceModule(sourcePath, module_description, varargin)
  runTests = false;
  if nargin == 3
    runTests = varargin{1};
  end
  steps = sourceSteps(runTests);
  stagingPath = newTemporaryDirectory();
  try
    copyfile(sourcePath, stagingPath, 'f');
    nmm_progress('install', module_description.module, module_description.version, 'build', steps.build, steps.total);
    run([stagingPath, 'builder.m']);
    if runTests
      nmm_progress('install', module_description.module, module_description.version, 'test', steps.test, steps.total);
      runPackageTests(stagingPath);
    end
    createModuleLockJson(stagingPath);
    nmm_progress('install', module_description.module, module_description.version, 'register', steps.register, steps.total);
    commitPreparedModule(stagingPath, module_description.module, module_description.version);
    cleanupDirectory(stagingPath);
    nmm_progress('install', module_description.module, module_description.version, 'done', steps.done, steps.total);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function runPackageTests(module_path)
  testFiles = packageTestFiles(module_path);
  if isempty(testFiles)
    nmm_error('PackageTestsMissing', 'No package tests found.');
  end
  status = test_run(testFiles, '-stoponfail');
  if ~status
    nmm_error('PackageTestsFailed', 'Package tests failed.');
  end
end
%=============================================================================
function testFiles = packageTestFiles(module_path)
  root = ensureTrailingSlash(module_path);
  entries = dir([root, 'tests/*.m']);
  testFiles = {};
  for k = 1:length(entries)
    if ~entries(k).isdir
      testFiles{end + 1} = [root, 'tests/', entries(k).name];
    end
  end
end
%=============================================================================
function destination_dir = commitPreparedModule(source, module_name, module_version)
  destination_dir = '';
  snapshot = installedModuleSnapshot(module_name);
  destination_dir = copyModule(source, module_name, module_version);
  try
    if ismodule(module_name)
      removemodule(module_name);
    end
    saveInstalledModule(module_name, destination_dir, module_version);
    modules = nmm_list();
    activeModule = getfield(modules, module_name);
    nmm_load(module_name, activeModule.version);
  catch
    e = lasterror();
    if isdir(destination_dir)
      cleanupDirectory(destination_dir);
    end
    restoreInstalledModuleSnapshot(module_name, snapshot);
    error(e);
  end
end
%=============================================================================
function destination_dir = copyModule(source, module_name, module_version)
  p = usermodulesdir();
  destination_dir = [p, module_name, '/', module_version, '/'];
  if isdir(destination_dir)
    nmm_prepare_destination(destination_dir, module_name, module_version);
  end
  mkdir(destination_dir);
  try
    copyfile(source, destination_dir, 'f');
    if ismodule('help_tools'), deployhelp('add', module_name, destination_dir); end
  catch
    e = lasterror();
    cleanupDirectory(destination_dir);
    error(e);
  end
end
%=============================================================================
function saveInstalledModule(module_name, module_path, version)
  element.path = module_path;
  element.load = true;
  element.version = version;
  modules = nmm_list();
  if isfield(modules, module_name)
    current = getfield(modules, module_name);
    versions = upsertInstalledVersion(installedVersions(current), element);
    activeVersion = selectedActiveVersion(current, versions);
    active = selectInstalledVersion(versions, activeVersion);
    active.versions = versions;
    if isfield(current, 'pinned_version') && hasInstalledVersion(versions, current.pinned_version)
      active.pinned_version = current.pinned_version;
    end
    modules.(module_name) = active;
  else
    element.versions = element;
    modules.(module_name) = element;
  end
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  txt = jsonprettyprint(jsonencode(modules));
  filewrite(modules_json_path, txt);
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
function versions = upsertInstalledVersion(versions, element)
  if isempty(versions)
    versions = element;
    return
  end
  for k = 1:length(versions)
    if strcmp(versions(k).version, element.version)
      versions(k) = element;
      return
    end
  end
  versions(end + 1) = element;
end
%=============================================================================
function version = selectedActiveVersion(current, versions)
  if isfield(current, 'pinned_version') && hasInstalledVersion(versions, current.pinned_version)
    version = current.pinned_version;
    return
  end
  version = latestInstalledVersion(versions);
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
function createModuleLockJson(module_path)
  info = nmm_read_module_json(module_path);
  moduleLockJson = [module_path, '/module-lock.json'];
  st = struct();
  st.module = info.module;
  st.version = info.version;
  st.lock = 2;
  st.lock_format = 'nelson.nmm.lock/2';
  if ~info.builtin
    st.platform = 'all';
  else
    st.platform = computer('arch');
  end
  st.builtin = info.builtin;
  st.package_type = packageType(info);
  source = struct();
  source.type = 'local';
  source.reference = '';
  st.source = source;
  nelson = struct();
  nelson.version = nelsonVersionString();
  nelson.arch = computer('arch');
  nelson.abi = nelsonAbiTag();
  st.nelson = nelson;
  st.checksums = lockChecksums(module_path);
  modules = readModulesJson();
  dependencies = struct();
  if isfield(info, 'dependencies')
    for m = fieldnames(info.dependencies)'
      if ~isfield(modules, m{1})
        nmm_error('DependencyNotInstalled', '%s not installed.', m{1});
      end
      dependencyInfo = getfield(modules, m{1});
      dependencies.(m{1}) = dependencyInfo.version;
    end
  end
  st.dependencies = dependencies;
  content = jsonprettyprint(jsonencode(st));
  filewrite(moduleLockJson, content);
end
%=============================================================================
function checksums = lockChecksums(module_path)
  root = ensureTrailingSlash(module_path);
  checksums = struct();
  checksums = addFileChecksum(checksums, 'module_json', [root, 'module.json']);
  checksums = addFileChecksum(checksums, 'loader', [root, 'loader.m']);
  checksums = addFileChecksum(checksums, 'builder', [root, 'builder.m']);
  checksums = addFileChecksum(checksums, 'etc_startup', [root, 'etc/startup.m']);
  checksums = addFileChecksum(checksums, 'etc_finish', [root, 'etc/finish.m']);
end
%=============================================================================
function checksums = addFileChecksum(checksums, field, filename)
  if isfile(filename)
    checksums.(field) = sha256(filename, '-file');
  end
end
%=============================================================================
function data = readModulesJson()
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  if isfile(modules_json_path)
    data = jsondecode(modules_json_path, '-file');
  else
    data = struct();
  end
end
%=============================================================================
function writeModulesJson(data)
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  filewrite(modules_json_path, jsonprettyprint(jsonencode(data)));
end
%=============================================================================
function s = nelsonVersionString()
  v = version('-number');
  s = sprintf('%d.%d.%d', v(1), v(2), v(3));
end
%=============================================================================
function tag = nelsonAbiTag()
  compiler = version('-compiler');
  tag = [computer('arch'), '-', compiler{3}];
end
%=============================================================================
function checkInstalledDependencies(info)
  if ~isfield(info, 'dependencies')
    return
  end
  for m = fieldnames(info.dependencies)'
    requiredVersion = info.dependencies.(m{1});
    if ~nmm_is_installed(m{1}, requiredVersion)
      nmm_error('DependencyNotInstalled', nmm_missing_dependency_message(m{1}));
    end
  end
end
%=============================================================================
function snapshot = installedModuleSnapshot(module_name)
  snapshot = struct();
  snapshot.exists = false;
  snapshot.data = struct();
  modules = nmm_list();
  if isfield(modules, module_name)
    snapshot.exists = true;
    snapshot.data = getfield(modules, module_name);
  end
end
%=============================================================================
function restoreInstalledModuleSnapshot(module_name, snapshot)
  if ismodule(module_name)
    try
      removemodule(module_name);
    catch
    end
  end
  try
    if ismodule('help_tools'), deployhelp('remove', module_name); end
  catch
  end
  modules = readModulesJson();
  if snapshot.exists
    modules.(module_name) = snapshot.data;
  elseif isfield(modules, module_name)
    modules = rmfield(modules, module_name);
  end
  writeModulesJson(modules);
  if snapshot.exists && isfield(snapshot.data, 'path') && isdir(snapshot.data.path)
    try
      nmm_load(module_name, snapshot.data.version);
    catch
    end
  end
end
%=============================================================================
function path = newTemporaryDirectory()
  root = [usermodulesdir(), '.nmm-tmp/'];
  if ~isdir(root)
    mkdir(root);
  end
  path = [root, createGUID(), '/'];
  mkdir(path);
end
%=============================================================================
function cleanupDirectory(path)
  if ischar(path) && isdir(path)
    [r, msg] = rmdir(path, 's');
  end
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
