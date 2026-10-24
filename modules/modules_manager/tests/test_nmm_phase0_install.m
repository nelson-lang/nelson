%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% <--WEIGHT 64-->
skip_testsuite(~ismodule('webtools'), 'webtools module is not installed.');
testRoot = [tempdir(), 'nmm_phase0_install/'];
externalModulesRoot = [testRoot, 'external_modules/'];
helpCacheRoot = [testRoot, 'help_cache/'];
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousHelpCacheDir = getenv('NELSON_HELP_CACHE_DIR');
depName = 'nmm_phase0_dep';
mainName = 'nmm_phase0_main';
lockedName = 'nmm_phase0_locked';
badName = 'nmm_phase0_bad';
autoDepName = 'nmm_phase0_auto_dep';
autoMainName = 'nmm_phase0_auto_main';
conflictName = 'nmm_phase0_conflict';
incompatibleName = 'nmm_phase0_incompatible';
noLoaderName = 'nmm_phase0_no_loader';
testFailName = 'nmm_phase0_test_fail';
missingDependencyName = 'nmm_phase0_missing_dep';
missingDependencyUserName = 'nmm_phase0_missing_dep_user';
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
mkdir(externalModulesRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', externalModulesRoot);
setenv('NELSON_HELP_CACHE_DIR', helpCacheRoot);
seedMinimalHelpCache(helpCacheRoot);
%=============================================================================
cleanupInstalledModule(mainName);
cleanupInstalledModule(lockedName);
cleanupInstalledModule(depName);
cleanupInstalledModule(badName);
cleanupInstalledModule(autoMainName);
cleanupInstalledModule(autoDepName);
cleanupInstalledModule(conflictName);
cleanupInstalledModule(incompatibleName);
cleanupInstalledModule(testFailName);
cleanupInstalledModule(missingDependencyUserName);
%=============================================================================
try
  depRoot = [testRoot, depName, '/'];
  depRootV2 = [testRoot, depName, '_2/'];
  mainRoot = [testRoot, mainName, '/'];
  mainFailedUpgradeRoot = [testRoot, mainName, '_failed_upgrade/'];
  lockedRoot = [testRoot, lockedName, '/'];
  badRoot = [testRoot, badName, '/'];
  autoDepRoot = [testRoot, autoDepName, '/'];
  autoMainRoot = [testRoot, autoMainName, '/'];
  conflictRoot = [testRoot, conflictName, '/'];
  incompatibleRoot = [testRoot, incompatibleName, '/'];
  noLoaderRoot = [testRoot, noLoaderName, '/'];
  testFailRoot = [testRoot, testFailName, '/'];
  missingDependencyUserRoot = [testRoot, missingDependencyUserName, '/'];
  createLocalModule(depRoot, depName, '1.0.0', '{}', false, '>=1.0.0');
  createLocalModule(depRootV2, depName, '2.0.0', '{}', false, '>=1.0.0');
  createLocalModule(mainRoot, mainName, '1.0.0', ['{"', depName, '":"1.0.0"}'], false, '>=1.0.0');
  createLocalModule(mainFailedUpgradeRoot, mainName, '2.0.0', ['{"', depName, '":"1.0.0"}'], false, '>=1.0.0');
  createLocalModule(lockedRoot, lockedName, '1.0.0', ['{"', depName, '":">=1.0.0"}'], false, '>=1.0.0');
  writeSourceLock(lockedRoot, lockedName, '1.0.0', depName, '1.0.0');
  createLocalModule(badRoot, badName, '1.0.0', '{}', true, '>=1.0.0');
  createLocalModule(autoDepRoot, autoDepName, '1.0.0', '{}', false, '>=1.0.0');
  createLocalModule(autoMainRoot, autoMainName, '1.0.0', ['{"', autoDepName, '":"', autoDepRoot, '"}'], false, '>=1.0.0');
  createLocalModule(conflictRoot, conflictName, '1.0.0', ['{"', depName, '":">=3.0.0"}'], false, '>=1.0.0');
  createLocalModule(incompatibleRoot, incompatibleName, '1.0.0', '{}', false, '>=999.0.0');
  createLocalModule(noLoaderRoot, noLoaderName, '1.0.0', '{}', false, '>=1.0.0');
  createLocalModule(testFailRoot, testFailName, '1.0.0', '{}', false, '>=1.0.0');
  createLocalModule(missingDependencyUserRoot, missingDependencyUserName, '1.0.0', ['{"', missingDependencyName, '":"1.0.0"}'], false, '>=1.0.0');
  filewrite([noLoaderRoot, 'builder.m'], '');
  filewrite([testFailRoot, 'tests/test_placeholder.m'], 'asserts.istrue(false);');
  filewrite([mainFailedUpgradeRoot, 'tests/test_placeholder.m'], 'asserts.istrue(false);');
  createOrphanDestination(depName, '1.0.0');

  nmm('install', depRoot);
  nmm('install', mainRoot);

  modules = nmm('list');
  asserts.istrue(isfield(modules, depName));
  asserts.istrue(isfield(modules, mainName));

  mainInfo = getfield(modules, mainName);
  lock = jsondecode([mainInfo.path, 'module-lock.json'], '-file');
  asserts.isequal(lock.lock, 2);
  asserts.isequal(lock.lock_format, 'nelson.nmm.lock/2');
  asserts.isequal(lock.package_type, 'source');
  asserts.isequal(lock.source.type, 'local');
  asserts.isequal(lock.nelson.arch, computer('arch'));
  asserts.istrue(isfield(lock.nelson, 'version'));
  asserts.istrue(isfield(lock.nelson, 'abi'));
  asserts.isequal(lock.dependencies.(depName), '1.0.0');
  asserts.istrue(isfield(lock, 'checksums'));
  asserts.isequal(lock.checksums.module_json, sha256([mainInfo.path, 'module.json'], '-file'));
  asserts.isequal(lock.checksums.loader, sha256([mainInfo.path, 'loader.m'], '-file'));
  asserts.isequal(lock.checksums.builder, sha256([mainInfo.path, 'builder.m'], '-file'));
  asserts.checkerror(['nmm(''install'', ''', mainFailedUpgradeRoot, ''', ''-tests'')'], _('Package tests failed.'), 'Nelson:nmm:PackageTestsFailed');
  modules = nmm('list');
  asserts.istrue(isfield(modules, mainName));
  asserts.isequal(modules.(mainName).version, '1.0.0');
  asserts.istrue(isdir(mainInfo.path));
  asserts.isequal(nmm_phase0_main_value(), 1);

  auditReport = nmm('audit');
  asserts.istrue(auditReport.ok);
  asserts.istrue(iscell(auditReport.issues));
  asserts.isequal(length(auditReport.issues), 0);

  moduleJsonFile = [mainInfo.path, 'module.json'];
  moduleJsonText = fileread(moduleJsonFile);
  filewrite(moduleJsonFile, strrep(moduleJsonText, 'nmm phase0 test module', 'nmm phase0 tampered module'));
  auditReport = nmm('audit');
  asserts.isfalse(auditReport.ok);
  asserts.istrue(issuesContain(auditReport.issues, 'module-lock.json checksum mismatch: module_json'));
  filewrite(moduleJsonFile, moduleJsonText);
  auditReport = nmm('audit');
  asserts.istrue(auditReport.ok);

  nmm('install', depRootV2);
  modules = nmm('list');
  asserts.isequal(modules.(depName).version, '2.0.0');

  nmm('install', lockedRoot);
  modules = nmm('list');
  lockedInfo = getfield(modules, lockedName);
  lockedLock = jsondecode([lockedInfo.path, 'module-lock.json'], '-file');
  asserts.isequal(lockedLock.dependencies.(depName), '1.0.0');

  nmm('install', autoMainRoot);
  modules = nmm('list');
  asserts.istrue(isfield(modules, autoDepName));
  asserts.istrue(isfield(modules, autoMainName));
  asserts.istrue(isfield(modules, depName));
  asserts.istrue(isfield(modules.(depName), 'versions'));
  asserts.isequal(length(modules.(depName).versions), 2);

  asserts.checkerror(['nmm(''install'', ''', conflictRoot, ''')'], 'nmm_phase0_conflict requires nmm_phase0_dep >=3.0.0, installed versions: 1.0.0, 2.0.0', 'Nelson:nmm:DependencyConflict');
  missingDependencyMessage = sprintf(_('%s not installed. Try nmm(''search'', ''%s'') to find it or nmm(''why'', ''%s'') to inspect installed reverse dependencies.'), missingDependencyName, missingDependencyName, missingDependencyName);
  asserts.checkerror(['nmm(''install'', ''', missingDependencyUserRoot, ''')'], missingDependencyMessage, 'Nelson:nmm:DependencyNotInstalled');

  asserts.checkerror(['nmm(''install'', ''', incompatibleRoot, ''')'], _('Nelson version not supported: >=999.0.0'), 'Nelson:nmm:NelsonVersionNotSupported');
  modules = nmm('list');
  asserts.isfalse(isfield(modules, incompatibleName));

  asserts.checkerror(['nmm(''install'', ''', badRoot, ''')'], 'builder failed');
  modules = nmm('list');
  asserts.isfalse(isfield(modules, badName));
  % package tests only run when requested: '-tests' fails, the default install succeeds
  asserts.checkerror(['nmm(''install'', ''', testFailRoot, ''', ''-tests'')'], _('Package tests failed.'), 'Nelson:nmm:PackageTestsFailed');
  modules = nmm('list');
  asserts.isfalse(isfield(modules, testFailName));
  nmm('install', testFailRoot);
  modules = nmm('list');
  asserts.istrue(isfield(modules, testFailName));

  if isfile([mainRoot, 'module-lock.json'])
    rmfile([mainRoot, 'module-lock.json']);
  end
  dryRunLock = nmm('lock', mainRoot, '-dry-run');
  asserts.isequal(dryRunLock.module, mainName);
  asserts.isequal(dryRunLock.dependencies.(depName), '1.0.0');
  asserts.isfalse(isfile([mainRoot, 'module-lock.json']));
  lock = nmm('lock', mainRoot);
  asserts.isequal(lock.module, mainName);
  asserts.isequal(lock.dependencies.(depName), '1.0.0');
  asserts.istrue(isfile([mainRoot, 'module-lock.json']));
  lockCheck = nmm('lock', mainRoot, '-check');
  asserts.istrue(lockCheck.ok);
  asserts.istrue(lockCheck.up_to_date);
  asserts.isequal(lockCheck.differences, {});
  lockDiff = nmm('lock', mainRoot, '-diff');
  asserts.istrue(lockDiff.ok);
  asserts.istrue(isfield(lockDiff, 'current'));
  asserts.istrue(isfield(lockDiff, 'expected'));
  verifySourceReport = nmm('verify', mainRoot);
  asserts.istrue(verifySourceReport.ok);
  asserts.istrue(verifySourceReport.up_to_date);
  asserts.isequal(verifySourceReport.module, mainName);
  mainRootModuleJson = [mainRoot, 'module.json'];
  mainRootModuleJsonText = fileread(mainRootModuleJson);
  filewrite(mainRootModuleJson, strrep(mainRootModuleJsonText, 'nmm phase0 test module', 'nmm phase0 source tampered module'));
  staleLockCheck = nmm('lock', mainRoot, '-check');
  asserts.isfalse(staleLockCheck.ok);
  asserts.isfalse(staleLockCheck.up_to_date);
  asserts.istrue(length(staleLockCheck.differences) > 0);
  asserts.checkerror(['nmm(''verify'', ''', mainRoot, ''')'], _('module-lock.json is not up to date.'), 'Nelson:nmm:ModuleLockOutOfDate');
  filewrite(mainRootModuleJson, mainRootModuleJsonText);
  nmm('lock', mainRoot);

  packageFile = nmm('package', mainName, testRoot);
  checksumFile = [packageFile, '.sha256'];
  asserts.istrue(isfile(checksumFile));
  asserts.isequal(strtrim(fileread(checksumFile)), sha256(packageFile, '-file'));
  assertPackageLockChecksums(packageFile, testRoot);
  verifyReport = nmm('verify', packageFile);
  asserts.istrue(verifyReport.ok);
  asserts.isequal(verifyReport.module, mainName);
  asserts.istrue(verifyReport.has_sha256);
  rmfile(checksumFile);
  verifyReport = nmm('verify', packageFile);
  asserts.istrue(verifyReport.ok);
  asserts.isfalse(verifyReport.has_sha256);
  dryRunReport = nmm('install', packageFile, '-dry-run');
  asserts.istrue(dryRunReport.ok);
  asserts.isequal(dryRunReport.module, mainName);
  nmm('install', packageFile);
  forceReport = nmm('install', packageFile, '-force');
  asserts.istrue(forceReport.ok);
  asserts.istrue(forceReport.forced);
  asserts.isequal(forceReport.module, mainName);
  asserts.isequal(nmm_phase0_main_value(), 1);
  filewrite(checksumFile, 'bad-checksum');
  asserts.checkerror(['nmm(''install'', ''', packageFile, ''')'], _('Package checksum does not match.'), 'Nelson:nmm:PackageChecksumMismatch');

  filewrite(checksumFile, sha256(packageFile, '-file'));
  tamperedPackage = tamperedPackageArchive(packageFile, testRoot);
  asserts.checkerror(['nmm(''install'', ''', tamperedPackage, ''')'], 'module-lock.json checksum mismatch: module_json', 'Nelson:nmm:ModuleLockChecksumMismatch');
  incompatiblePackage = incompatiblePackageArchive(packageFile, testRoot);
  asserts.checkerror(['nmm(''install'', ''', incompatiblePackage, ''')'], _('Nelson version not supported: >=999.0.0'), 'Nelson:nmm:NelsonVersionNotSupported');
  incompatibleAbiPackage = incompatibleAbiPackageArchive(packageFile, testRoot);
  asserts.checkerror(['nmm(''install'', ''', incompatibleAbiPackage, ''')'], _('Nelson ABI not supported: incompatible-abi'), 'Nelson:nmm:NelsonAbiNotSupported');
  asserts.checkerror(['nmm(''pack'', ''', noLoaderRoot, ''', ''', testRoot, ''')'], _('loader.m is missing.'), 'Nelson:nmm:LoaderMissing');
  asserts.checkerror(['nmm(''pack'', ''', testFailRoot, ''', ''', testRoot, ''')'], _('Package tests failed.'), 'Nelson:nmm:PackageTestsFailed');
  packDryRunReport = nmm('pack', mainRoot, '-dry-run');
  asserts.istrue(packDryRunReport.ok);
  asserts.istrue(packDryRunReport.dry_run);
  asserts.isequal(packDryRunReport.module, mainName);
  asserts.isequal(packDryRunReport.package_file, '');
  packDryRunWithDestination = nmm('pack', mainRoot, testRoot, '-dry-run');
  asserts.istrue(packDryRunWithDestination.ok);
  asserts.istrue(endsWith(packDryRunWithDestination.package_file, [mainName, '-all-1.0.0.nmz']));
  asserts.isfalse(isfile(packDryRunWithDestination.package_file));
  noTestsArchive = nmm('pack', testFailRoot, testRoot, '-no-tests');
  asserts.istrue(isfile(noTestsArchive));

  cleanupInstalledModule(mainName);
  cleanupInstalledModule(lockedName);
  cleanupInstalledModule(depName);
  cleanupInstalledModule(badName);
  cleanupInstalledModule(autoMainName);
  cleanupInstalledModule(autoDepName);
  cleanupInstalledModule(conflictName);
  cleanupInstalledModule(incompatibleName);
  cleanupInstalledModule(testFailName);
  cleanupInstalledModule(missingDependencyUserName);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  rmdir(testRoot, 's');
catch e
  cleanupInstalledModule(mainName);
  cleanupInstalledModule(lockedName);
  cleanupInstalledModule(depName);
  cleanupInstalledModule(badName);
  cleanupInstalledModule(autoMainName);
  cleanupInstalledModule(autoDepName);
  cleanupInstalledModule(conflictName);
  cleanupInstalledModule(incompatibleName);
  cleanupInstalledModule(testFailName);
  cleanupInstalledModule(missingDependencyUserName);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
  rethrow(e);
end
%=============================================================================
function restoreExternalModulesPath(value)
  setenv('NELSON_EXTERNAL_MODULES_PATH', value);
end
%=============================================================================
function restoreHelpCacheDir(value)
  setenv('NELSON_HELP_CACHE_DIR', value);
end
%=============================================================================
function seedMinimalHelpCache(root)
  for lang = getavailablelanguages()'
    langDir = [root, lang{1}, '/'];
    if ~isdir(langDir)
      mkdir(langDir);
    end
    filewrite([langDir, 'index.html'], '<!doctype html><html><body></body></html>');
    filewrite([langDir, 'index.json'], '{}');
  end
end
%=============================================================================
function createLocalModule(moduleRoot, moduleName, moduleVersion, dependenciesJson, failBuild, nelsonRange)
  mkdir(moduleRoot);
  mkdir([moduleRoot, 'etc']);
  mkdir([moduleRoot, 'functions']);
  mkdir([moduleRoot, 'help']);
  mkdir([moduleRoot, 'help/en_US']);
  mkdir([moduleRoot, 'help/en_US/xml']);
  mkdir([moduleRoot, 'tests']);

  moduleJson = [
    "{";
    ['  "module": "', moduleName, '",'];
    ['  "title": "', moduleName, '",'];
    ['  "summary": "nmm phase0 test module",'];
    ['  "version": "', moduleVersion, '",'];
    ['  "platforms": ["all"],'];
    ['  "nelson": "', nelsonRange, '",'];
    ['  "builtin": false,'];
    ['  "license": "BSD-3-Clause",'];
    ['  "dependencies": ', dependenciesJson];
    "}"
  ];
  filewrite([moduleRoot, 'module.json'], moduleJson);

  if failBuild
    builder = "error('builder failed');";
  else
    builder = [
      "moduleRoot = fileparts(nfilename('fullpath'));";
      ['nmm_build_loader(''', moduleName, ''', moduleRoot);']
    ];
  end
  filewrite([moduleRoot, 'builder.m'], builder);

  startup = ['addpath([modulepath(''', moduleName, '''), ''/functions'']);'];
  finish = ['rmpath([modulepath(''', moduleName, '''), ''/functions'']);'];
  filewrite([moduleRoot, 'etc/startup.m'], startup);
  filewrite([moduleRoot, 'etc/finish.m'], finish);
  filewrite([moduleRoot, 'functions/', moduleName, '_value.m'], ['function r = ', moduleName, '_value()', newline, '  r = 1;', newline, 'end']);
  filewrite([moduleRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
  filewrite([moduleRoot, 'tests/test_placeholder.m'], '');
end
%=============================================================================
function createOrphanDestination(moduleName, moduleVersion)
  root = [usermodulesdir(), moduleName, '/', moduleVersion, '/'];
  if isdir(root)
    return
  end
  parent = [usermodulesdir(), moduleName];
  if ~isdir(parent)
    mkdir(parent);
  end
  mkdir(root);
  moduleJson = [
    "{";
    ['  "module": "', moduleName, '",'];
    ['  "version": "', moduleVersion, '"'];
    "}"
  ];
  filewrite([root, 'module.json'], moduleJson);
end
%=============================================================================
function writeSourceLock(moduleRoot, moduleName, moduleVersion, dependencyName, dependencyVersion)
  content = [
    "{";
    ['  "module": "', moduleName, '",'];
    ['  "version": "', moduleVersion, '",'];
    "  ""dependencies"": {";
    ['    "', dependencyName, '": "', dependencyVersion, '"'];
    "  }";
    "}"
  ];
  filewrite([moduleRoot, 'module-lock.json'], content);
end
%=============================================================================
function packageFile = tamperedPackageArchive(packageFile, testRoot)
  staging = [testRoot, 'tampered_archive/'];
  if isdir(staging)
    rmdir(staging, 's');
  end
  mkdir(staging);
  unzip(packageFile, staging);
  moduleJsonFile = [staging, 'module.json'];
  moduleInfo = jsondecode(moduleJsonFile, '-file');
  moduleInfo.version = '1.2.3';
  moduleInfo.summary = 'tampered module descriptor';
  filewrite(moduleJsonFile, jsonprettyprint(jsonencode(moduleInfo)));
  lockFile = [staging, 'module-lock.json'];
  lock = jsondecode(lockFile, '-file');
  lock.version = '1.2.3';
  filewrite(lockFile, jsonprettyprint(jsonencode(lock)));
  packageFile = [testRoot, 'tampered-module.nmz'];
  zip(packageFile, staging, staging);
  rmdir(staging, 's');
end
%=============================================================================
function packageFile = incompatiblePackageArchive(packageFile, testRoot)
  staging = [testRoot, 'incompatible_archive/'];
  if isdir(staging)
    rmdir(staging, 's');
  end
  mkdir(staging);
  unzip(packageFile, staging);
  moduleJsonFile = [staging, 'module.json'];
  text = fileread(moduleJsonFile);
  text = strrep(text, '>=1.0.0', '>=999.0.0');
  filewrite(moduleJsonFile, text);
  packageFile = [testRoot, 'incompatible-module.nmz'];
  zip(packageFile, staging, staging);
  filewrite([packageFile, '.sha256'], sha256(packageFile, '-file'));
  rmdir(staging, 's');
end
%=============================================================================
function packageFile = incompatibleAbiPackageArchive(packageFile, testRoot)
  staging = [testRoot, 'incompatible_abi_archive/'];
  if isdir(staging)
    rmdir(staging, 's');
  end
  mkdir(staging);
  unzip(packageFile, staging);
  moduleJsonFile = [staging, 'module.json'];
  moduleInfo = jsondecode(moduleJsonFile, '-file');
  moduleInfo.version = '9.9.9';
  moduleInfo.builtin = true;
  filewrite(moduleJsonFile, jsonprettyprint(jsonencode(moduleInfo)));
  lockFile = [staging, 'module-lock.json'];
  lock = jsondecode(lockFile, '-file');
  lock.version = '9.9.9';
  lock.builtin = true;
  lock.package_type = 'binary';
  lock.nelson.abi = 'incompatible-abi';
  filewrite(lockFile, jsonprettyprint(jsonencode(lock)));
  packageFile = [testRoot, 'incompatible-abi-module.nmz'];
  zip(packageFile, staging, staging);
  filewrite([packageFile, '.sha256'], sha256(packageFile, '-file'));
  rmdir(staging, 's');
end
%=============================================================================
function assertPackageLockChecksums(packageFile, testRoot)
  staging = [testRoot, 'package_lock_check/'];
  if isdir(staging)
    rmdir(staging, 's');
  end
  mkdir(staging);
  unzip(packageFile, staging);
  lock = jsondecode([staging, 'module-lock.json'], '-file');
  asserts.istrue(isfield(lock, 'checksums'));
  asserts.isequal(lock.package_type, 'source');
  asserts.isequal(lock.checksums.module_json, sha256([staging, 'module.json'], '-file'));
  asserts.isequal(lock.checksums.loader, sha256([staging, 'loader.m'], '-file'));
  rmdir(staging, 's');
end
%=============================================================================
function cleanupInstalledModule(moduleName)
  try
    modules = nmm('list');
    if isfield(modules, moduleName)
      nmm('uninstall', moduleName);
    end
  catch
  end
end
%=============================================================================
function tf = issuesContain(issues, pattern)
  tf = false;
  for k = 1:length(issues)
    if contains(issues{k}, pattern)
      tf = true;
      return
    end
  end
end
%=============================================================================
