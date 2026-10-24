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
% A binary package can ship one archive per architecture under a single
% registry entry (same name and version). Publishing win64 then woa64 (or the
% reverse) accumulates both under "artifacts"; installing resolves the archive
% for the running architecture.
%=============================================================================
skip_testsuite(~ismodule('webtools'), 'webtools module is not installed.');
moduleName = 'nmm_multiarch_fixture';
moduleVersion = '1.0.0';
currentArch = computer('arch');
otherArch = 'woa64';
if strcmp(currentArch, otherArch)
  otherArch = 'win64';
end

testRoot = [tempdir(), 'nmm_registry_multiarch/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
registryFile = [testRoot, 'registry.json'];

previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousSigningKey = getenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
setenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
setenv('NELSON_NMM_REGISTRY', registryFile);

currentArchive = buildBinaryPackage(testRoot, moduleName, moduleVersion, currentArch);
otherArchive = buildBinaryPackage(testRoot, moduleName, moduleVersion, otherArch);

% first publish: the running architecture
firstReport = nmm('publish', currentArchive);
asserts.istrue(firstReport.published);

info = nmm('info', moduleName, moduleVersion);
asserts.istrue(isfield(info, 'artifacts'));
asserts.istrue(isfield(info.artifacts, currentArch));
asserts.isequal(info.artifacts.(currentArch).source, fullpath(currentArchive));
asserts.isequal(info.artifacts.(currentArch).checksum, sha256(currentArchive, '-file'));
% the entry for the running architecture is flattened into source/checksum so
% the installer sees a plain hosted archive
asserts.isequal(info.source, fullpath(currentArchive));
asserts.isequal(info.checksum, sha256(currentArchive, '-file'));
asserts.istrue(hasTextValue(info.platforms, currentArch));
asserts.isfalse(hasTextValue(info.platforms, otherArch));

% second publish: a different architecture of the SAME version is an addition,
% not a duplicate, so it needs no -force. -source stores the hosted URL while
% the checksum still comes from the local archive.
otherUrl = ['https://example.org/', moduleName, '-', otherArch, '-', moduleVersion, '.nmz'];
secondReport = nmm('publish', otherArchive, '-source', otherUrl);
asserts.istrue(secondReport.published);

info = nmm('info', moduleName, moduleVersion);
asserts.istrue(isfield(info.artifacts, currentArch));
asserts.istrue(isfield(info.artifacts, otherArch));
asserts.isequal(info.artifacts.(otherArch).source, otherUrl);
asserts.isequal(info.artifacts.(otherArch).checksum, sha256(otherArchive, '-file'));
asserts.istrue(hasTextValue(info.platforms, currentArch));
asserts.istrue(hasTextValue(info.platforms, otherArch));
% still resolved for the running architecture
asserts.isequal(info.source, fullpath(currentArchive));
asserts.isequal(info.checksum, sha256(currentArchive, '-file'));

% a single registry entry holds both architectures, so doctor sees no duplicate
% and no missing source
doctorReport = nmm('registry', 'doctor');
asserts.isequal(doctorReport.duplicates, {});
asserts.isequal(doctorReport.missing_sources, {});
asserts.isequal(doctorReport.inconsistent_artifacts, {});
asserts.isequal(doctorReport.stats.entries, 1);
asserts.istrue(hasTextValue(doctorReport.stats.platforms, currentArch));
asserts.istrue(hasTextValue(doctorReport.stats.platforms, otherArch));

% re-publishing an architecture already present needs -force
duplicateMessage = sprintf(_('Package already exists in registry: %s %s. Use -force to replace it.'), moduleName, moduleVersion);
asserts.checkerror(['nmm(''publish'', ''', currentArchive, ''')'], duplicateMessage, 'Nelson:nmm:PackageAlreadyExists');
asserts.checkerror(['nmm(''publish'', ''', otherArchive, ''')'], duplicateMessage, 'Nelson:nmm:PackageAlreadyExists');

% -force replaces only that architecture and keeps the other one
forcedReport = nmm('publish', currentArchive, '-force');
asserts.istrue(forcedReport.published);
asserts.istrue(forcedReport.forced);
info = nmm('info', moduleName, moduleVersion);
asserts.istrue(isfield(info.artifacts, currentArch));
asserts.istrue(isfield(info.artifacts, otherArch));

% quick win #2: -source URL whose architecture token contradicts the packed
% archive is rejected (checked without mutating the registry)
otherArchUrl = ['https://example.org/', moduleName, '-', otherArch, '-', moduleVersion, '.nmz'];
asserts.checkerror(['nmm(''publish'', ''', currentArchive, ''', ''-source'', ''', otherArchUrl, ''', ''-check'')'], ...
  sprintf(_('The -source URL references architecture %s but the package is %s.'), otherArch, currentArch), ...
  'Nelson:nmm:RegistrySourceUrlArchMismatch');

% quick win #1: doctor flags an entry whose platforms and artifacts disagree
inconsistentRegistry = [testRoot, 'inconsistent.json'];
inconsistent = struct();
inconsistent.name = 'inconsistent_fixture';
inconsistent.version = '1.0.0';
inconsistent.platforms = {currentArch, otherArch};
inconsistent.artifacts = struct();
inconsistent.artifacts.(currentArch) = struct('source', 'https://example.org/a.nmz', 'checksum', 'deadbeef');
writeRegistryFile(inconsistentRegistry, inconsistent);
setenv('NELSON_NMM_REGISTRY', inconsistentRegistry);
inconsistentDoctor = nmm('registry', 'doctor');
asserts.isfalse(inconsistentDoctor.ok);
asserts.istrue(hasTextValue(inconsistentDoctor.inconsistent_artifacts, 'inconsistent_fixture@1.0.0'));

% quick win #3: installing an architecture that has no artifact fails with a
% clear message even when the platform gate would pass
missingArchRegistry = [testRoot, 'missing_arch.json'];
missingArch = struct();
missingArch.name = 'missing_arch_fixture';
missingArch.version = '1.0.0';
missingArch.platforms = {currentArch, otherArch};
missingArch.artifacts = struct();
missingArch.artifacts.(otherArch) = struct('source', 'https://example.org/o.nmz', 'checksum', 'feedface');
writeRegistryFile(missingArchRegistry, missingArch);
setenv('NELSON_NMM_REGISTRY', missingArchRegistry);
asserts.checkerror('nmm(''install'', ''missing_arch_fixture'', ''1.0.0'', ''-dry-run'')', ...
  sprintf(_('No %s archive available for %s %s.'), currentArch, 'missing_arch_fixture', '1.0.0'), ...
  'Nelson:nmm:ArchiveNotAvailable');

% opt-in -platform filter: the core search reports every registry entry, while
% -platform keeps only the packages installable on the running architecture
platformRegistry = [testRoot, 'platform.json'];
localPkg = struct('name', 'local_arch_fixture', 'version', '1.0.0', 'platforms', {{currentArch}}, 'summary', 'installable here');
foreignPkg = struct('name', 'foreign_arch_fixture', 'version', '1.0.0', 'platforms', {{otherArch}}, 'summary', 'other architecture only');
portablePkg = struct('name', 'portable_fixture', 'version', '1.0.0', 'platforms', {{'all'}}, 'summary', 'portable');
entries(1) = localPkg;
entries(2) = foreignPkg;
entries(3) = portablePkg;
writeRegistryFile(platformRegistry, entries);
setenv('NELSON_NMM_REGISTRY', platformRegistry);
allResults = nmm('registry', 'search', '');
asserts.isequal(numel(allResults), 3);
platformResults = nmm('registry', 'search', '', '-platform');
platformNames = {};
for k = 1:numel(platformResults)
  platformNames{end + 1} = platformResults(k).name;
end
asserts.isequal(numel(platformResults), 2);
asserts.istrue(any(strcmp(platformNames, 'local_arch_fixture')));
asserts.istrue(any(strcmp(platformNames, 'portable_fixture')));
asserts.isfalse(any(strcmp(platformNames, 'foreign_arch_fixture')));
% same through the top-level 'search' dispatch
asserts.isequal(numel(nmm('search', '', '-platform')), 2);

setenv('NELSON_NMM_REGISTRY', previousRegistry);
setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', previousSigningKey);
%=============================================================================
function writeRegistryFile(registryFile, package)
  registry = struct();
  registry.packages = package;
  filewrite(registryFile, jsonprettyprint(jsonencode(registry)));
  filewrite([registryFile, '.sha256'], [sha256(registryFile, '-file'), '  ', 'registry.json']);
end
%=============================================================================
function archive = buildBinaryPackage(testRoot, moduleName, moduleVersion, arch)
  staging = [testRoot, moduleName, '_', arch, '/'];
  if isdir(staging)
    rmdir(staging, 's');
  end
  mkdir(staging);
  moduleJson = struct();
  moduleJson.module = moduleName;
  moduleJson.title = moduleName;
  moduleJson.version = moduleVersion;
  moduleJson.summary = 'multi-arch fixture';
  moduleJson.description = 'multi-arch registry fixture';
  moduleJson.license = 'BSD-3-Clause';
  moduleJson.authors = {'Nelson Tests'};
  moduleJson.repository = 'https://example.org/nmm_multiarch_fixture.git';
  moduleJson.keywords = {'registry', 'multiarch'};
  moduleJson.builtin = true;
  moduleJson.platforms = {'win64', 'woa64'};
  filewrite([staging, 'module.json'], jsonprettyprint(jsonencode(moduleJson)));
  lockJson = struct();
  lockJson.module = moduleName;
  lockJson.version = moduleVersion;
  lockJson.platform = arch;
  lockJson.builtin = true;
  lockJson.package_type = 'binary';
  nelson = struct();
  nelson.version = '2.0.0';
  nelson.arch = arch;
  nelson.abi = [arch, '-test'];
  lockJson.nelson = nelson;
  lockJson.dependencies = struct();
  filewrite([staging, 'module-lock.json'], jsonprettyprint(jsonencode(lockJson)));
  filewrite([staging, 'loader.m'], '% fixture loader');
  archive = [testRoot, moduleName, '-', arch, '-', moduleVersion, '.nmz'];
  if isfile(archive)
    rmfile(archive);
  end
  zip(archive, staging, staging);
  filewrite([archive, '.sha256'], sha256(archive, '-file'));
end
%=============================================================================
function tf = hasTextValue(container, value)
  tf = false;
  if ischar(container)
    tf = strcmp(container, value);
    return
  end
  if iscell(container)
    for k = 1:numel(container)
      if ischar(container{k}) && strcmp(container{k}, value)
        tf = true;
        return
      end
    end
  end
end
%=============================================================================
