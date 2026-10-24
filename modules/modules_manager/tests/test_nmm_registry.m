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
testRoot = [tempdir(), 'nmm_registry/'];
externalModulesRoot = [testRoot, 'external_modules/'];
helpCacheRoot = [testRoot, 'help_cache/'];
registryModuleName = 'nmm_registry_module';
registryDependencyName = 'nmm_registry_dependency';
registryDependentName = 'nmm_registry_dependent';
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
mkdir(externalModulesRoot);
registryFile = [testRoot, 'registry.json'];
registryModuleRoot = [testRoot, registryModuleName, '_1_0_0/'];
registryModuleRootLatest = [testRoot, registryModuleName, '_1_1_0/'];
registryRequiredName = 'nmm_registry_required';
registryRequiredRoot = [testRoot, registryRequiredName, '_1_0_0/'];
registryDependencyRoot = [testRoot, registryDependencyName, '_1_0_0/'];
registryDependentRoot = [testRoot, registryDependentName, '_1_0_0/'];
unsupportedArch = 'glnxa64';
if strcmp(computer('arch'), unsupportedArch)
  unsupportedArch = 'win64';
end
previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousSigningKey = getenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousHelpCacheDir = getenv('NELSON_HELP_CACHE_DIR');
setenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
setenv('NELSON_EXTERNAL_MODULES_PATH', externalModulesRoot);
setenv('NELSON_HELP_CACHE_DIR', helpCacheRoot);
seedMinimalHelpCache(helpCacheRoot);
%=============================================================================
try
  cleanupInstalledModule(registryModuleName);
  cleanupInstalledModule(registryDependencyName);
  cleanupInstalledModule(registryDependentName);
  createLocalModule(registryModuleRoot, registryModuleName, '1.0.0');
  createLocalModule(registryModuleRootLatest, registryModuleName, '1.1.0');
  % a failing package test, published with "tests": "required"
  cleanupInstalledModule(registryRequiredName);
  createLocalModule(registryRequiredRoot, registryRequiredName, '1.0.0');
  filewrite([registryRequiredRoot, 'tests/test_placeholder.m'], 'error(''fixture failure'');');
  createLocalModule(registryDependencyRoot, registryDependencyName, '1.0.0');
  createLocalModule(registryDependentRoot, registryDependentName, '1.0.0', ['{"', registryDependencyName, '": "^1.0.0"}']);
  nmm_cache_dir = nmm('cache', 'dir');
  nmm('cache', 'clear');
  nmm('install', registryModuleRoot);
  registryArchive = nmm('package', registryModuleName, testRoot);
  registryArchiveChecksum = sha256(registryArchive, '-file');
  nmm('uninstall', registryModuleName);
  registry = [
    "{";
    "  ""packages"": [";
    "    {";
    "      ""name"": ""signal_tools"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Signal helpers"",";
    "      ""description"": ""Signal processing helpers"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": ""abc"",";
    "      ""signature"": """",";
    ['      "source": "', registryModuleRoot, '"'];
    "    },";
    "    {";
    "      ""name"": ""signal_tools"",";
    "      ""version"": ""1.1.0"",";
    "      ""summary"": ""Signal helpers"",";
    "      ""description"": ""Signal processing helpers"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": ""def"",";
    "      ""signature"": """",";
    ['      "source": "', registryModuleRoot, '"'];
    "    },";
    "    {";
    "      ""name"": ""image_tools"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Image helpers"",";
    "      ""description"": ""Image processing helpers"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": ""ghi"",";
    "      ""signature"": """",";
    ['      "source": "', registryModuleRoot, '"'];
    "    },";
    "    {";
    "      ""name"": ""bad_platform_tools"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Unsupported platform fixture"",";
    "      ""description"": ""Unsupported platform fixture"",";
    ['      "platforms": ["', unsupportedArch, '"],'];
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    "      ""source"": ""missing-source""";
    "    },";
    "    {";
    "      ""name"": ""bad_nelson_tools"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Unsupported Nelson fixture"",";
    "      ""description"": ""Unsupported Nelson fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=999.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    "      ""source"": ""missing-source""";
    "    },";
    "    {";
    ['      "name": "', registryDependencyName, '",'];
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Registry dependency fixture"",";
    "      ""description"": ""Registry dependency fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    ['      "source": "', registryDependencyRoot, '"'];
    "    },";
    "    {";
    ['      "name": "', registryDependentName, '",'];
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Registry dependent fixture"",";
    "      ""description"": ""Registry dependent fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    ['      "dependencies": {"', registryDependencyName, '": "^1.0.0"},'];
    "      ""checksum"": """",";
    "      ""signature"": """",";
    ['      "source": "', registryDependentRoot, '"'];
    "    },";
    "    {";
    ['      "name": "', registryModuleName, '",'];
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Registry install fixture"",";
    "      ""description"": ""Registry install fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    ['      "checksum": "', registryArchiveChecksum, '",'];
    "      ""signature"": """",";
    ['      "source": "', registryArchive, '"'];
    "    },";
    "    {";
    ['      "name": "', registryModuleName, '",'];
    "      ""version"": ""1.1.0"",";
    "      ""summary"": ""Registry install fixture"",";
    "      ""description"": ""Registry install fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": ""mno"",";
    "      ""signature"": """",";
    ['      "source": "', registryModuleRootLatest, '"'];
    "    },";
    "    {";
    ['      "name": "', registryRequiredName, '",'];
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Registry install fixture (tests required)"",";
    "      ""description"": ""Registry install fixture (tests required)"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    "      ""tests"": ""required"",";
    ['      "source": "', registryRequiredRoot, '"'];
    "    },";
    "    {";
    "      ""name"": ""signal_tools"",";
    "      ""version"": ""1.2.0"",";
    "      ""summary"": ""Signal helpers (yanked)"",";
    "      ""description"": ""Signal processing helpers, withdrawn"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    "      ""yanked"": true,";
    "      ""yank_reason"": ""superseded"",";
    ['      "source": "', registryModuleRoot, '"'];
    "    },";
    "    {";
    "      ""name"": ""remote_archive"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Hosted archive fixture"",";
    "      ""description"": ""Hosted archive fixture"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": """",";
    "      ""signature"": """",";
    "      ""source"": ""https://example.invalid/remote_archive-1.0.0.nmz""";
    "    }";
    "  ]";
    "}"
  ];
  filewrite(registryFile, registry);
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
  setenv('NELSON_NMM_REGISTRY', registryFile);

  configReport = nmm('config');
  asserts.isequal(configReport.registry, registryFile);
  asserts.isequal(configReport.cache_dir, nmm_cache_dir);
  asserts.isequal(configReport.offline_cache_count, 0);
  asserts.isfalse(configReport.signing_key_configured);

  registryCheck = nmm('registry', 'check');
  asserts.istrue(registryCheck.ok);
  asserts.istrue(registryCheck.checksum_ok);
  registryValidate = nmm('registry', 'validate');
  asserts.istrue(registryValidate.ok);
  registryStats = nmm('registry', 'stats');
  asserts.isequal(registryStats.entries, 12);
  asserts.isequal(registryStats.packages, 9);
  asserts.isequal(registryStats.versions, 12);
  asserts.isequal(registryStats.checksummed, 5);
  asserts.istrue(hasTextValue(registryStats.platforms, 'all'));
  registryList = nmm('registry', 'list');
  asserts.isequal(registryList.registry, registryFile);
  asserts.isequal(length(registryList.packages), 12);
  asserts.isequal(registryList.stats.entries, registryStats.entries);
  registryDoctor = nmm('registry', 'doctor');
  asserts.isfalse(registryDoctor.ok);
  asserts.isequal(registryDoctor.stats.entries, 12);
  asserts.isequal(registryDoctor.duplicates, {});
  asserts.istrue(hasTextValue(registryDoctor.missing_sources, 'bad_platform_tools@1.0.0'));
  asserts.istrue(hasTextValue(registryDoctor.missing_sources, 'bad_nelson_tools@1.0.0'));

  rmfile([registryFile, '.sha256']);
  registryCheck = nmm('registry', 'check');
  asserts.isfalse(registryCheck.ok);
  asserts.istrue(hasTextValue(registryCheck.issues, _('Registry checksum file is missing.')));
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry checksum file is missing.'), 'Nelson:nmm:RegistryChecksumMissing');
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));

  found = nmm('search', 'signal');
  asserts.isequal(length(found), 2);
  asserts.isequal(found(1).name, 'signal_tools');
  foundJson = jsondecode(nmm('search', 'signal', '-json'));
  asserts.isequal(length(foundJson), 2);

  info = nmm('info', 'signal_tools');
  asserts.isequal(info.name, 'signal_tools');
  asserts.isequal(info.version, '1.1.0');
  infoJson = jsondecode(nmm('info', 'signal_tools', '-json'));
  asserts.isequal(infoJson.version, '1.1.0');

  info = nmm('info', 'signal_tools', '1.0.0');
  asserts.isequal(info.version, '1.0.0');
  infoVersionJson = jsondecode(nmm('info', 'signal_tools', '1.0.0', '-json'));
  asserts.isequal(infoVersionJson.version, '1.0.0');

  info = nmm('info', 'signal_tools', '>=1.0.0 <2.0.0');
  asserts.isequal(info.version, '1.1.0');
  latestInfo = nmm('latest', 'signal_tools');
  asserts.isequal(latestInfo.version, '1.1.0');
  resolveInfo = nmm('resolve', 'signal_tools', '>=1.0.0 <1.1.0');
  asserts.isequal(resolveInfo.package, 'signal_tools');
  asserts.isequal(resolveInfo.version, '1.0.0');
  asserts.isequal(resolveInfo.requested, '>=1.0.0 <1.1.0');
  asserts.istrue(nmm('satisfies', '1.2.0', '>=1.0.0 <2.0.0'));
  asserts.isfalse(nmm('satisfies', '2.0.0', '>=1.0.0 <2.0.0'));

  versions = nmm('versions', 'signal_tools');
  asserts.isequal(versions, {'1.0.0', '1.1.0'});
  versionsJson = jsondecode(nmm('versions', 'signal_tools', '-json'));
  asserts.isequal(length(versionsJson), 2);
  asserts.istrue(hasTextValue(versionsJson, '1.0.0'));
  asserts.istrue(hasTextValue(versionsJson, '1.1.0'));

  asserts.checkerror('nmm(''info'', ''missing_package'')', _('Package not found: missing_package'), 'Nelson:nmm:PackageNotFound');
  expectedPlatformError = sprintf(_('Platform not supported: %s'), unsupportedArch);
  asserts.checkerror('nmm(''install'', ''bad_platform_tools'', ''1.0.0'')', expectedPlatformError, 'Nelson:nmm:PlatformNotSupported');
  asserts.checkerror('nmm(''install'', ''bad_nelson_tools'', ''1.0.0'')', _('Nelson version not supported: >=999.0.0'), 'Nelson:nmm:NelsonVersionNotSupported');

  dryRunRegistryInstall = nmm('install', registryDependentName, '1.0.0', '-dry-run');
  asserts.istrue(dryRunRegistryInstall.ok);
  asserts.isequal(dryRunRegistryInstall.package, registryDependentName);
  asserts.isequal(dryRunRegistryInstall.version, '1.0.0');
  asserts.istrue(planContains(dryRunRegistryInstall.install_plan, registryDependentName, 'install'));
  asserts.istrue(planContains(dryRunRegistryInstall.install_plan, registryDependencyName, 'install'));
  whyPlanReport = nmm('why', dryRunRegistryInstall, registryDependencyName);
  asserts.istrue(whyPlanReport.in_plan);
  asserts.istrue(hasTextValue(whyPlanReport.required_by, registryDependentName));
  asserts.istrue(hasTextValue(whyPlanReport.chains, [registryDependentName, ' -> ', registryDependencyName]));
  whyPlanJson = jsondecode(nmm('why', dryRunRegistryInstall, registryDependencyName, '-json'));
  asserts.istrue(whyPlanJson.in_plan);
  asserts.istrue(hasTextValue(whyPlanJson.required_by, registryDependentName));
  treePlanReport = nmm('tree', dryRunRegistryInstall);
  asserts.isequal(treePlanReport.package, registryDependentName);
  asserts.isequal(treePlanReport.action, 'install');
  asserts.isequal(length(treePlanReport.dependencies), 1);
  asserts.isequal(treePlanReport.dependencies(1).package, registryDependencyName);
  asserts.isequal(treePlanReport.dependencies(1).requested, '^1.0.0');
  asserts.isequal(treePlanReport.missing, {});
  asserts.isequal(treePlanReport.conflicts, {});
  treePlanJson = jsondecode(nmm('tree', dryRunRegistryInstall, '-json'));
  asserts.isequal(treePlanJson.package, registryDependentName);
  asserts.isequal(treePlanJson.dependencies(1).package, registryDependencyName);
  depsPlanReport = nmm('deps', dryRunRegistryInstall);
  asserts.isequal(length(depsPlanReport), 1);
  asserts.isequal(depsPlanReport(1).package, registryDependencyName);
  asserts.isequal(depsPlanReport(1).requested, '^1.0.0');
  asserts.isequal(depsPlanReport(1).action, 'install');
  depsPlanJson = jsondecode(nmm('deps', dryRunRegistryInstall, '-json'));
  asserts.isequal(depsPlanJson(1).package, registryDependencyName);
  modules = nmm('list');
  asserts.isfalse(isfield(modules, registryDependencyName));
  asserts.isfalse(isfield(modules, registryDependentName));

  nmm('install', registryDependentName, '1.0.0');
  modules = nmm('list');
  asserts.istrue(isfield(modules, registryDependencyName));
  asserts.istrue(isfield(modules, registryDependentName));
  asserts.isequal(modules.(registryDependencyName).version, '1.0.0');
  asserts.isequal(modules.(registryDependentName).version, '1.0.0');
  whyInstalledReport = nmm('why', registryDependencyName);
  asserts.istrue(whyInstalledReport.installed);
  asserts.istrue(hasTextValue(whyInstalledReport.required_by, registryDependentName));
  asserts.isfalse(whyInstalledReport.direct);
  whyInstalledJson = jsondecode(nmm('why', registryDependencyName, '-json'));
  asserts.istrue(whyInstalledJson.installed);
  asserts.istrue(hasTextValue(whyInstalledJson.required_by, registryDependentName));
  directWhyReport = nmm('why', registryDependentName);
  asserts.istrue(directWhyReport.installed);
  asserts.istrue(directWhyReport.direct);
  treeInstalledReport = nmm('tree', registryDependentName);
  asserts.isequal(treeInstalledReport.package, registryDependentName);
  asserts.isequal(treeInstalledReport.version, '1.0.0');
  asserts.istrue(treeInstalledReport.installed);
  asserts.isequal(length(treeInstalledReport.dependencies), 1);
  asserts.isequal(treeInstalledReport.dependencies(1).package, registryDependencyName);
  asserts.isequal(treeInstalledReport.dependencies(1).version, '1.0.0');
  asserts.istrue(treeInstalledReport.dependencies(1).installed);
  asserts.isequal(treeInstalledReport.missing, {});
  treeInstalledJson = jsondecode(nmm('tree', registryDependentName, '-json'));
  asserts.isequal(treeInstalledJson.package, registryDependentName);
  asserts.isequal(treeInstalledJson.dependencies(1).package, registryDependencyName);
  depsInstalledReport = nmm('deps', registryDependentName);
  asserts.isequal(length(depsInstalledReport), 1);
  asserts.isequal(depsInstalledReport(1).package, registryDependencyName);
  asserts.isequal(depsInstalledReport(1).version, '1.0.0');
  asserts.istrue(depsInstalledReport(1).installed);
  depsInstalledJson = jsondecode(nmm('deps', registryDependentName, '-json'));
  asserts.isequal(depsInstalledJson(1).package, registryDependencyName);
  rdepsReport = nmm('rdeps', registryDependencyName);
  asserts.isequal(length(rdepsReport), 1);
  asserts.isequal(rdepsReport(1).package, registryDependentName);
  asserts.isequal(rdepsReport(1).version, '1.0.0');
  asserts.isequal(rdepsReport(1).dependency, registryDependencyName);
  asserts.isequal(rdepsReport(1).requested, '1.0.0');
  statusReport = nmm('status', registryDependentName);
  asserts.istrue(statusReport.installed);
  asserts.istrue(statusReport.ok);
  asserts.isequal(statusReport.version, '1.0.0');
  asserts.istrue(statusReport.autoload);
  asserts.isequal(length(statusReport.dependencies), 1);
  asserts.isequal(statusReport.dependencies(1).package, registryDependencyName);
  asserts.isequal(statusReport.missing, {});
  auditTargetReport = nmm('audit', registryDependentName);
  asserts.istrue(auditTargetReport.ok);
  asserts.isequal(auditTargetReport.package, registryDependentName);
  asserts.isequal(auditTargetReport.issues, {});
  doctorTargetReport = nmm('doctor', registryDependentName);
  asserts.istrue(doctorTargetReport.ok);
  asserts.isequal(doctorTargetReport.package, registryDependentName);
  asserts.istrue(doctorTargetReport.status.installed);
  asserts.istrue(doctorTargetReport.status.ok);
  doctorTargetJson = jsondecode(nmm('doctor', registryDependentName, '-json'));
  asserts.istrue(doctorTargetJson.ok);
  asserts.isequal(doctorTargetJson.package, registryDependentName);
  asserts.istrue(doctorTargetJson.status.installed);
  explainReport = nmm('explain', registryDependentName);
  asserts.isequal(explainReport.package, registryDependentName);
  asserts.istrue(explainReport.status.installed);
  asserts.isequal(explainReport.tree.package, registryDependentName);
  asserts.isequal(explainReport.why.package, registryDependentName);
  asserts.istrue(hasTextValue(explainReport.summary, [registryDependentName, ' 1.0.0 is installed.']));
  graphReport = nmm('graph', registryDependentName);
  asserts.isequal(graphReport.package, registryDependentName);
  asserts.istrue(contains(graphReport.dot, ['"', registryDependentName, '@1.0.0"']));
  asserts.istrue(contains(graphReport.dot, ['"', registryDependencyName, '@1.0.0"']));
  asserts.istrue(contains(graphReport.dot, ' -> '));
  asserts.istrue(contains(graphReport.mermaid, 'graph TD'));
  missingStatusReport = nmm('status', 'nmm_missing_status_fixture');
  asserts.isfalse(missingStatusReport.installed);
  asserts.isfalse(missingStatusReport.ok);
  dryRunRemoveDependency = nmm('remove', registryDependencyName, '-dry-run');
  asserts.istrue(dryRunRemoveDependency.dry_run);
  asserts.isfalse(dryRunRemoveDependency.allowed);
  asserts.isequal(length(dryRunRemoveDependency.required_by), 1);
  asserts.isequal(dryRunRemoveDependency.required_by(1).package, registryDependentName);
  asserts.checkerror(['nmm(''remove'', ''', registryDependencyName, ''')'], sprintf(_('Package is required by installed modules: %s. Use -force to remove it.'), registryDependencyName), 'Nelson:nmm:PackageRequired');
  removeDependencyReport = nmm('remove', registryDependencyName, '-force');
  asserts.istrue(removeDependencyReport.removed);
  asserts.istrue(removeDependencyReport.forced);
  asserts.isequal(length(removeDependencyReport.required_by), 1);
  treeMissingReport = nmm('tree', registryDependentName);
  asserts.isequal(length(treeMissingReport.dependencies), 1);
  asserts.isequal(treeMissingReport.dependencies(1).package, registryDependencyName);
  asserts.isfalse(treeMissingReport.dependencies(1).installed);
  asserts.isequal(treeMissingReport.dependencies(1).action, 'missing');
  asserts.istrue(hasTextValue(treeMissingReport.missing, [registryDependencyName, '@1.0.0']));
  statusMissingDependencyReport = nmm('status', registryDependentName);
  asserts.isfalse(statusMissingDependencyReport.ok);
  asserts.istrue(hasTextValue(statusMissingDependencyReport.missing, [registryDependencyName, '@1.0.0']));
  auditMissingDependencyReport = nmm('audit', registryDependentName);
  asserts.isfalse(auditMissingDependencyReport.ok);
  asserts.istrue(contains(auditMissingDependencyReport.issues{1}, 'dependency is not installed'));
  doctorMissingDependencyReport = nmm('doctor', registryDependentName);
  asserts.isfalse(doctorMissingDependencyReport.ok);
  asserts.isfalse(doctorMissingDependencyReport.status.ok);
  nmm('remove', registryDependentName);

  nmm('install', registryDependentName, '1.0.0');
  modules = nmm('list');
  asserts.istrue(modules.(registryDependencyName).installed_as_dependency);
  asserts.istrue(modules.(registryDependencyName).versions(1).installed_as_dependency);
  nmm('remove', registryDependentName);
  orphanReport = nmm('orphans');
  asserts.isequal(length(orphanReport), 1);
  asserts.isequal(orphanReport(1).package, registryDependencyName);
  asserts.isequal(orphanReport(1).version, '1.0.0');
  orphanJson = jsondecode(nmm('orphans', '-json'));
  asserts.isequal(orphanJson(1).package, registryDependencyName);
  dryRunAutoremoveReport = nmm('autoremove', '-dry-run');
  asserts.istrue(dryRunAutoremoveReport.dry_run);
  asserts.isfalse(dryRunAutoremoveReport.changed);
  asserts.isequal(dryRunAutoremoveReport.removed(1).package, registryDependencyName);
  asserts.istrue(isfield(nmm('list'), registryDependencyName));
  autoremoveJson = jsondecode(nmm('autoremove', '-dry-run', '-json'));
  asserts.istrue(autoremoveJson.dry_run);
  autoremoveReport = nmm('autoremove');
  asserts.isfalse(autoremoveReport.dry_run);
  asserts.istrue(autoremoveReport.changed);
  asserts.isequal(autoremoveReport.removed(1).package, registryDependencyName);
  asserts.isfalse(isfield(nmm('list'), registryDependencyName));

  registryText = fileread(registryFile);
  filewrite(registryFile, strrep(registryText, registryArchiveChecksum, '0000'));
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
  asserts.checkerror(['nmm(''install'', ''', registryModuleName, ''', ''1.0.0'')'], _('Registry checksum does not match package source.'), 'Nelson:nmm:RegistryPackageChecksumMismatch');
  filewrite(registryFile, registryText);
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));

  dryRunLatestRegistryInstall = nmm('install', registryModuleName, '-dry-run');
  asserts.istrue(dryRunLatestRegistryInstall.ok);
  asserts.isequal(dryRunLatestRegistryInstall.package, registryModuleName);
  asserts.isequal(dryRunLatestRegistryInstall.version, '1.1.0');
  asserts.isfalse(isfield(nmm('list'), registryModuleName));
  nmm('install', registryModuleName, '1.1.0', '-no-tests');
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.1.0');
  nmm('install', registryModuleName, '1.1.0', '-force');
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.1.0');
  nmm('remove', registryModuleName);

  % "tests": "required" in the registry entry always runs the package tests,
  % even without -tests and even with -no-tests
  asserts.checkerror(['nmm(''install'', ''', registryRequiredName, ''', ''1.0.0'')'], _('Package tests failed.'), 'Nelson:nmm:PackageTestsFailed');
  asserts.checkerror(['nmm(''install'', ''', registryRequiredName, ''', ''1.0.0'', ''-no-tests'')'], _('Package tests failed.'), 'Nelson:nmm:PackageTestsFailed');
  asserts.isfalse(isfield(nmm('list'), registryRequiredName));

  % a yanked version is hidden from search, versions and "latest", stays
  % resolvable when requested explicitly, and is never installed anew
  yankedInfo = nmm('registry', 'info', 'signal_tools', '1.2.0');
  asserts.istrue(logical(yankedInfo.yanked));
  asserts.isequal(nmm('registry', 'info', 'signal_tools').version, '1.1.0');
  asserts.isfalse(any(strcmp(nmm('registry', 'versions', 'signal_tools'), '1.2.0')));
  yankedMessage = sprintf(_('Package version has been yanked from the registry: %s %s%s'), 'signal_tools', '1.2.0', ': superseded');
  asserts.checkerror('nmm(''install'', ''signal_tools'', ''1.2.0'')', yankedMessage, 'Nelson:nmm:PackageYanked');

  % a hosted .nmz source must declare a checksum in the registry (Gap #1)
  asserts.checkerror('nmm(''install'', ''remote_archive'', ''1.0.0'')', _('A hosted .nmz package must declare a checksum in the registry.'), 'Nelson:nmm:RegistryPackageChecksumRequired');

  dryRunRegistryArchiveInstall = nmm('install', registryModuleName, '1.0.0', '-dry-run');
  asserts.istrue(dryRunRegistryArchiveInstall.ok);
  asserts.isequal(dryRunRegistryArchiveInstall.package, registryModuleName);
  asserts.isequal(dryRunRegistryArchiveInstall.version, '1.0.0');
  asserts.istrue(dryRunRegistryArchiveInstall.install_plan(1).checksum_verified);
  asserts.isfalse(isfield(nmm('list'), registryModuleName));
  asserts.isequal(length(nmm('cache', 'list')), 0);

  nmm('install', registryModuleName, '1.0.0');
  modules = nmm('list');
  asserts.istrue(isfield(modules, registryModuleName));
  asserts.isequal(modules.(registryModuleName).version, '1.0.0');
  cachedArchive = nmm('cache', 'package', registryModuleName, '1.0.0');
  asserts.istrue(isfile(cachedArchive));
  cachedPackages = nmm('cache', 'list');
  asserts.istrue(cacheContains(cachedPackages, registryModuleName, '1.0.0'));
  cacheSize = nmm('cache', 'size');
  asserts.isequal(cacheSize.archives, 1);
  asserts.istrue(cacheSize.bytes > 0);
  cacheInfo = nmm('cache', 'info', registryModuleName, '1.0.0');
  asserts.istrue(cacheInfo.exists);
  asserts.istrue(cacheInfo.has_sha256);
  asserts.istrue(cacheInfo.checksum_ok);
  asserts.isequal(cacheInfo.bytes, fileSize(cachedArchive));
  asserts.istrue(nmm('cache', 'has', registryModuleName, '1.0.0'));
  asserts.isfalse(nmm('cache', 'has', registryModuleName, '9.9.9'));
  cacheExportDir = [testRoot, 'cache_export/'];
  cacheExportReport = nmm('cache', 'export', cacheExportDir);
  asserts.isequal(cacheExportReport.destination, cacheExportDir);
  asserts.isequal(length(cacheExportReport.packages), 1);
  asserts.istrue(isfile([cacheExportDir, registryModuleName, '-1.0.0.nmz']));
  asserts.istrue(isfile([cacheExportDir, registryModuleName, '-1.0.0.nmz.sha256']));
  nmm('cache', 'remove', registryModuleName, '1.0.0');
  asserts.isfalse(nmm('cache', 'has', registryModuleName, '1.0.0'));
  cacheImportReport = nmm('cache', 'import', cacheExportDir);
  asserts.isequal(length(cacheImportReport.packages), 1);
  asserts.istrue(nmm('cache', 'has', registryModuleName, '1.0.0'));
  nmm('cache', 'remove', registryModuleName, '1.0.0');
  cacheAddReport = nmm('cache', 'add', [cacheExportDir, registryModuleName, '-1.0.0.nmz']);
  asserts.istrue(cacheAddReport.added);
  asserts.isequal(cacheAddReport.module, registryModuleName);
  asserts.isequal(cacheAddReport.version, '1.0.0');
  asserts.istrue(nmm('cache', 'has', registryModuleName, '1.0.0'));

  nmm('uninstall', registryModuleName);
  rmfile(registryArchive);
  rmfile([registryArchive, '.sha256']);
  setenv('NELSON_NMM_REGISTRY', [testRoot, 'missing_registry.json']);
  missingRegistryMessage = _('Package registry is not configured. Set NELSON_NMM_REGISTRY or create registry.json in usermodulesdir(). Local source, archive and offline cache commands remain available.');
  asserts.checkerror('nmm(''search'', ''signal'')', missingRegistryMessage, 'Nelson:nmm:RegistryNotConfigured');
  asserts.checkerror('nmm(''install'', ''missing_cached_package'', ''-offline'')', _('Package is not available in cache.'), 'Nelson:nmm:CachePackageNotFound');
  offlineDryRun = nmm('install', registryModuleName, '1.0.0', '-offline', '-dry-run');
  asserts.istrue(offlineDryRun.ok);
  asserts.istrue(offlineDryRun.offline);
  asserts.isequal(offlineDryRun.module, registryModuleName);
  asserts.isequal(offlineDryRun.version, '1.0.0');
  asserts.isfalse(isfield(nmm('list'), registryModuleName));
  nmm('install', registryModuleName, '1.0.0', '-offline');
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.0.0');
  nmm('uninstall', registryModuleName);
  offlineLatestDryRun = nmm('install', registryModuleName, '-offline', '-dry-run');
  asserts.istrue(offlineLatestDryRun.ok);
  asserts.istrue(offlineLatestDryRun.offline);
  asserts.isequal(offlineLatestDryRun.version, '1.0.0');
  nmm('install', registryModuleName, '-offline');
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.0.0');
  setenv('NELSON_NMM_REGISTRY', registryFile);

  outdated = nmm('outdated', registryModuleName);
  asserts.isequal(length(outdated), 1);
  asserts.isequal(outdated(1).current, '1.0.0');
  asserts.isequal(outdated(1).latest, '1.1.0');
  outdatedJson = jsondecode(nmm('outdated', registryModuleName, '-json'));
  asserts.isequal(outdatedJson(1).current, '1.0.0');
  asserts.isequal(outdatedJson(1).latest, '1.1.0');
  dryRunUpdate = nmm('update', registryModuleName, '-dry-run');
  asserts.istrue(dryRunUpdate.dry_run);
  asserts.isequal(dryRunUpdate.updates(1).latest, '1.1.0');
  globalDryRunUpdate = nmm('update', '-dry-run');
  asserts.istrue(globalDryRunUpdate.dry_run);
  asserts.isequal(globalDryRunUpdate.updates(1).module, registryModuleName);
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.0.0');

  updateReport = nmm('update', registryModuleName);
  asserts.isequal(updateReport.updated, {registryModuleName});
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.1.0');
  asserts.isequal(length(nmm('outdated', registryModuleName)), 0);

  dryRunRemoveReport = nmm('remove', registryModuleName, '-dry-run');
  asserts.istrue(dryRunRemoveReport.dry_run);
  asserts.istrue(dryRunRemoveReport.allowed);
  asserts.isfalse(dryRunRemoveReport.removed);
  asserts.istrue(length(dryRunRemoveReport.paths) >= 1);
  removeReport = nmm('remove', registryModuleName);
  asserts.istrue(removeReport.removed);
  asserts.istrue(removeReport.changed);
  asserts.isfalse(isfield(nmm('list'), registryModuleName));

  writeBrokenModulesJson(registryModuleName, [testRoot, 'missing_path/']);
  doctorReport = nmm('doctor');
  asserts.isfalse(doctorReport.ok);
  asserts.istrue(contains(doctorReport.issues{1}, 'installed path does not exist'));
  asserts.istrue(isfield(doctorReport, 'nelson_version'));
  asserts.istrue(isfield(doctorReport, 'usermodulesdir'));
  asserts.istrue(isfield(doctorReport, 'registry'));
  asserts.istrue(isfield(doctorReport, 'cache_dir'));
  asserts.istrue(hasTextValue(doctorReport.broken_modules, registryModuleName));
  asserts.istrue(hasTextValue(doctorReport.suggestions, 'run nmm(''repair'')'));
  doctorJson = jsondecode(nmm('doctor', '-json'));
  asserts.isfalse(doctorJson.ok);
  asserts.istrue(hasTextValue(doctorJson.broken_modules, registryModuleName));
  auditJson = nmm('audit', '-json');
  auditReport = jsondecode(auditJson);
  asserts.isfalse(auditReport.ok);
  repairReport = nmm('repair');
  asserts.isequal(repairReport.removed, {[registryModuleName, '@1.0.0']});
  asserts.istrue(nmm('doctor').ok);
  writeBrokenModulesJson(registryModuleName, [testRoot, 'missing_path/']);
  nmm('remove', registryModuleName);
  asserts.istrue(nmm('doctor').ok);

  packDryRunReport = nmm('pack', registryModuleRootLatest, '-dry-run');
  asserts.istrue(packDryRunReport.ok);
  asserts.istrue(packDryRunReport.dry_run);
  asserts.isequal(packDryRunReport.module, registryModuleName);
  asserts.isequal(packDryRunReport.version, '1.1.0');
  asserts.isequal(packDryRunReport.package_file, '');
  quietPackDryRunText = evalc('quietPackDryRunReport = nmm(''pack'', registryModuleRootLatest, ''-dry-run'', ''-quiet'');');
  asserts.isequal(strtrim(quietPackDryRunText), '');
  asserts.istrue(quietPackDryRunReport.ok);
  asserts.istrue(quietPackDryRunReport.dry_run);
  packArchive = nmm('pack', registryModuleRootLatest, testRoot);
  asserts.istrue(isfile(packArchive));
  asserts.istrue(isfile([packArchive, '.sha256']));
  asserts.isequal(strtrim(fileread([packArchive, '.sha256'])), sha256(packArchive, '-file'));
  registryBeforeDryRunPublish = fileread(registryFile);
  checkPublishReport = nmm('publish', packArchive, '-check');
  asserts.isfalse(checkPublishReport.published);
  asserts.istrue(checkPublishReport.checked);
  asserts.istrue(checkPublishReport.existing);
  asserts.isfalse(checkPublishReport.publishable);
  checkPublishJson = jsondecode(nmm('publish', packArchive, '-check', '-force', '-json'));
  asserts.istrue(checkPublishJson.checked);
  asserts.istrue(checkPublishJson.publishable);
  asserts.istrue(checkPublishJson.existing);
  dryRunPublishReport = nmm('publish', packArchive, '-dry-run');
  asserts.isfalse(dryRunPublishReport.published);
  asserts.istrue(dryRunPublishReport.dry_run);
  asserts.isequal(dryRunPublishReport.package, registryModuleName);
  asserts.isfalse(dryRunPublishReport.publishable);
  asserts.isequal(fileread(registryFile), registryBeforeDryRunPublish);
  duplicatePublishMessage = sprintf(_('Package already exists in registry: %s %s. Use -force to replace it.'), registryModuleName, '1.1.0');
  asserts.checkerror(['nmm(''publish'', ''', packArchive, ''')'], duplicatePublishMessage, 'Nelson:nmm:PackageAlreadyExists');
  publishReport = nmm('publish', packArchive, '-force');
  asserts.istrue(publishReport.published);
  asserts.istrue(publishReport.forced);
  registryChecksumFile = [registryFile, '.sha256'];
  asserts.istrue(isfile(registryChecksumFile));
  asserts.isequal(strtrim(fileread(registryChecksumFile)), sha256(registryFile, '-file'));
  publishedInfo = nmm('info', registryModuleName, '1.1.0');
  asserts.isequal(publishedInfo.checksum, sha256(packArchive, '-file'));
  asserts.isequal(publishedInfo.summary, 'registry install fixture');
  asserts.isequal(publishedInfo.description, 'Registry module fixture description');
  asserts.isequal(publishedInfo.license, 'BSD-3-Clause');
  asserts.isequal(publishedInfo.repository, 'https://example.org/nmm_registry_module.git');
  asserts.isequal(publishedInfo.package_type, 'source');
  asserts.istrue(hasTextValue(publishedInfo.authors, 'Nelson Tests'));
  asserts.istrue(hasTextValue(publishedInfo.keywords, 'registry'));
  asserts.istrue(hasTextValue(publishedInfo.keywords, 'nmm'));
  asserts.istrue(hasTextValue(publishedInfo.tags, 'registry'));
  asserts.istrue(hasTextValue(publishedInfo.tags, 'nmm'));
  registryText = fileread(registryFile);
  filewrite(registryFile, [registryText, newline, ' ']);
  asserts.checkerror(['nmm(''info'', ''', registryModuleName, ''', ''1.1.0'')'], _('Registry checksum does not match.'), 'Nelson:nmm:RegistryChecksumMismatch');
  filewrite(registryFile, registryText);
  filewrite(registryChecksumFile, sha256(registryFile, '-file'));
  nmm('install', registryModuleName, '1.1.0');
  modules = nmm('list');
  asserts.isequal(modules.(registryModuleName).version, '1.1.0');
  cacheReport = nmm('cache', 'verify');
  asserts.istrue(cacheReport.ok);
  dryRunPruneReport = nmm('cache', 'prune', '-dry-run');
  asserts.istrue(dryRunPruneReport.dry_run);
  asserts.isequal(length(dryRunPruneReport.removed), 1);
  cachedPackages = nmm('cache', 'list');
  asserts.istrue(cacheContains(cachedPackages, registryModuleName, '1.0.0'));
  asserts.istrue(cacheContains(cachedPackages, registryModuleName, '1.1.0'));
  pruneReport = nmm('cache', 'prune');
  asserts.isfalse(pruneReport.dry_run);
  asserts.isequal(length(pruneReport.removed), 1);
  cachedPackages = nmm('cache', 'list');
  asserts.istrue(cacheContains(cachedPackages, registryModuleName, '1.1.0'));
  asserts.isfalse(cacheContains(cachedPackages, registryModuleName, '1.0.0'));
  cachedPath = nmm('cache', 'package', registryModuleName, '1.1.0');
  filewrite([cachedPath, '.sha256'], 'bad-checksum');
  cacheReport = nmm('cache', 'verify');
  asserts.isfalse(cacheReport.ok);
  asserts.isequal(length(cacheReport.corrupted), 1);
  filewrite([cachedPath, '.sha256'], sha256(cachedPath, '-file'));
  removeCacheReport = nmm('cache', 'remove', registryModuleName, '1.1.0');
  asserts.istrue(removeCacheReport.removed);
  asserts.isfalse(isfile(cachedPath));
  asserts.isfalse(isfile([cachedPath, '.sha256']));
  nmm('remove', registryModuleName);

  % Ed25519 signing seed (RFC 8032 test vector, public by nature)
  testSeed = 'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7';
  [~, testPublicKey] = crypto.ed25519.sign(uint8([]), testSeed);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', testSeed);
  publishReport = nmm('publish', packArchive, '-force');
  asserts.istrue(publishReport.published);
  registrySignatureFile = [registryFile, '.sig'];
  asserts.istrue(isfile(registrySignatureFile));
  registrySignature = fileread(registrySignatureFile);
  asserts.isfalse(isempty(registrySignature));
  signatureDocument = jsondecode(registrySignature);
  asserts.isequal(signatureDocument.signatures(1).public_key, testPublicKey);
  asserts.istrue(crypto.ed25519.verify(registryFile, signatureDocument.signatures(1).signature, testPublicKey, '-file'));
  publishedInfo = nmm('info', registryModuleName, '1.1.0');
  asserts.isequal(publishedInfo.checksum, sha256(packArchive, '-file'));
  filewrite(registrySignatureFile, 'bad-signature');
  asserts.checkerror(['nmm(''info'', ''', registryModuleName, ''', ''1.1.0'')'], sprintf(_('Registry signature file is not valid: %s'), registrySignatureFile), 'Nelson:nmm:RegistrySignatureFormatInvalid');
  filewrite(registrySignatureFile, registrySignature);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
  asserts.checkerror(['nmm(''info'', ''', registryModuleName, ''', ''1.1.0'')'], sprintf(_('Registry signature key is not trusted: %s'), testPublicKey), 'Nelson:nmm:RegistrySignatureUntrustedKey');
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', testPublicKey);
  publishedInfo = nmm('info', registryModuleName, '1.1.0');
  asserts.isequal(publishedInfo.checksum, sha256(packArchive, '-file'));
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', '');
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', testSeed);
  rmfile(registrySignatureFile);
  % a local registry without signature sidecar stays readable (development registry)
  publishedInfo = nmm('info', registryModuleName, '1.1.0');
  asserts.isequal(publishedInfo.checksum, sha256(packArchive, '-file'));

  cleanupInstalledModule(registryModuleName);
  cleanupInstalledModule(registryDependencyName);
  cleanupInstalledModule(registryDependentName);
  restoreSigningKey(previousSigningKey);
  restoreRegistry(previousRegistry);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  rmdir(testRoot, 's');
catch e
  cleanupInstalledModule(registryModuleName);
  cleanupInstalledModule(registryDependencyName);
  cleanupInstalledModule(registryDependentName);
  restoreSigningKey(previousSigningKey);
  restoreRegistry(previousRegistry);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
  rethrow(e);
end
%=============================================================================
function createLocalModule(moduleRoot, moduleName, moduleVersion, varargin)
  dependenciesJson = '{}';
  if nargin == 4
    dependenciesJson = varargin{1};
  end
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
    ['  "summary": "registry install fixture",'];
    ['  "description": "Registry module fixture description",'];
    ['  "version": "', moduleVersion, '",'];
    ['  "platforms": ["all"],'];
    ['  "nelson": ">=1.0.0",'];
    ['  "builtin": false,'];
    ['  "license": "BSD-3-Clause",'];
    ['  "authors": ["Nelson Tests"],'];
    ['  "repository": "https://example.org/', moduleName, '.git",'];
    ['  "keywords": ["registry", "nmm"],'];
    ['  "dependencies": ', dependenciesJson];
    "}"
  ];
  filewrite([moduleRoot, 'module.json'], moduleJson);
  builder = [
    "moduleRoot = fileparts(nfilename('fullpath'));";
    ['nmm_build_loader(''', moduleName, ''', moduleRoot);']
  ];
  filewrite([moduleRoot, 'builder.m'], builder);
  filewrite([moduleRoot, 'etc/startup.m'], '');
  filewrite([moduleRoot, 'etc/finish.m'], '');
  filewrite([moduleRoot, 'functions/', moduleName, '_value.m'], ['function r = ', moduleName, '_value()', newline, '  r = 1;', newline, 'end']);
  filewrite([moduleRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
  filewrite([moduleRoot, 'tests/test_placeholder.m'], '');
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
function writeBrokenModulesJson(moduleName, modulePath)
  element.path = modulePath;
  element.load = true;
  element.version = '1.0.0';
  modules = struct();
  modules.(moduleName) = element;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
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
function tf = hasTextValue(values, expected)
  tf = false;
  if ischar(values)
    tf = strcmp(values, expected);
    return
  end
  if iscell(values)
    for k = 1:length(values)
      if ischar(values{k}) && strcmp(values{k}, expected)
        tf = true;
        return
      end
    end
  end
end
%=============================================================================
function tf = cacheContains(packages, moduleName, moduleVersion)
  tf = false;
  for k = 1:length(packages)
    if strcmp(packages(k).name, moduleName) && strcmp(packages(k).version, moduleVersion)
      tf = true;
      return
    end
  end
end
%=============================================================================
function bytes = fileSize(filename)
  bytes = 0;
  info = dir(filename);
  if ~isempty(info)
    bytes = info(1).bytes;
  end
end
%=============================================================================
function tf = planContains(plan, packageName, action)
  tf = false;
  for k = 1:length(plan)
    if strcmp(plan(k).package, packageName) && strcmp(plan(k).action, action)
      tf = true;
      return
    end
  end
end
%=============================================================================
function restoreRegistry(previousRegistry)
  if isempty(previousRegistry)
    setenv('NELSON_NMM_REGISTRY');
  else
    setenv('NELSON_NMM_REGISTRY', previousRegistry);
  end
end
%=============================================================================
function restoreSigningKey(previousSigningKey)
  if isempty(previousSigningKey)
    setenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
  else
    setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', previousSigningKey);
  end
end
%=============================================================================
