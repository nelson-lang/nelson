%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_publish(varargin)
  if length(varargin) < 1 || length(varargin) > 6
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  package_file = varargin{1};
  if ~ischar(package_file)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~isfile(package_file)
    nmm_error('FilenameExpected', 'filename expected.');
  end
  if ~endsWith(package_file, '.nmz')
    nmm_error('InvalidPackageFileExtension', 'filename extension .nmz expected.');
  end
  options = parseOptions(varargin(2:end));
  verifyPackageChecksum(package_file);
  registryFile = registryFilename();
  [lock, moduleInfo] = packageMetadata(package_file);
  if ~isempty(options.source)
    validateSourceUrl(options.source, lock.platform, lock.version);
  end
  registry = readLocalRegistry(registryFile);
  entry = packageEntry(lock, moduleInfo, package_file, options.source);
  exists = hasPackage(registry.packages, entry.name, entry.version);
  duplicate = entryAlreadyPublished(registry.packages, entry);
  if options.check || options.dry_run
    report = publishReport(false, options.dry_run, options.force, registryFile, entry, exists);
    report.checked = options.check;
    report.publishable = ~duplicate || options.force;
    if ~report.publishable
      report.errors = {duplicatePackageMessage(entry.name, entry.version)};
    else
      report.errors = {};
    end
    report = encodeReport(report, options.json);
    return
  end
  if duplicate && ~options.force
    nmm_error('PackageAlreadyExists', duplicatePackageMessage(entry.name, entry.version));
  end
  registry.packages = upsertPackage(registry.packages, entry);
  filewrite(registryFile, jsonprettyprint(jsonencode(registry)));
  writeRegistryChecksum(registryFile);
  nmm_registry_signature('write', registryFile);
  report = publishReport(true, false, options.force && exists, registryFile, entry, exists);
  report.checked = false;
  report.publishable = true;
  report.errors = {};
  report = encodeReport(report, options.json);
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.dry_run = false;
  options.check = false;
  options.force = false;
  options.json = false;
  options.source = '';
  k = 1;
  while k <= length(args)
    option = args{k};
    if ~ischar(option)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
    end
    switch option
      case '-dry-run'
        options.dry_run = true;
      case '-check'
        options.check = true;
      case '-force'
        options.force = true;
      case '-json'
        options.json = true;
      case '-source'
        % Hosted registries (e.g. GitHub releases) reference the archive by
        % URL, not by the local path where it was packed. -source <url> stores
        % that URL as the archive source while the checksum is still taken from
        % the local file being published.
        if k == length(args)
          nmm_error('RegistrySourceUrlExpected', 'A source URL is expected after ''-source''.');
        end
        value = args{k + 1};
        if ~ischar(value) || ~(startsWith(value, 'http://') || startsWith(value, 'https://'))
          nmm_error('RegistrySourceUrlInvalid', '''-source'' expects an http(s) URL.');
        end
        options.source = value;
        k = k + 1;
      otherwise
        nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-dry-run'', ''-check'', ''-force'', ''-source'' or ''-json''', 3));
    end
    k = k + 1;
  end
end
%=============================================================================
function report = publishReport(published, dryRun, forced, registryFile, entry, exists)
  report = struct();
  report.published = published;
  report.dry_run = dryRun;
  report.forced = forced;
  report.existing = exists;
  report.registry = registryFile;
  report.package = entry.name;
  report.version = entry.version;
  report.entry = entry;
end
%=============================================================================
function report = encodeReport(report, jsonOutput)
  if jsonOutput
    report = jsonencode(report);
  end
end
%=============================================================================
function tf = hasPackage(packages, name, version)
  tf = false;
  if isempty(packages)
    return
  end
  for k = 1:length(packages)
    if strcmp(packages(k).name, name) && strcmp(packages(k).version, version)
      tf = true;
      return
    end
  end
end
%=============================================================================
function msg = duplicatePackageMessage(name, version)
  msg = sprintf(_('Package already exists in registry: %s %s. Use -force to replace it.'), name, version);
end
%=============================================================================
function verifyPackageChecksum(package_file)
  checksum_file = [package_file, '.sha256'];
  if ~isfile(checksum_file)
    return
  end
  expected = strtrim(fileread(checksum_file));
  actual = sha256(package_file, '-file');
  if ~strcmpi(expected, actual)
    nmm_error('PackageChecksumMismatch', 'Package checksum does not match.');
  end
end
%=============================================================================
function registryFile = registryFilename()
  registryFile = getenv('NELSON_NMM_REGISTRY');
  if isempty(registryFile)
    registryFile = [usermodulesdir(), 'registry.json'];
  end
  if startsWith(registryFile, 'http://') || startsWith(registryFile, 'https://')
    nmm_error('LocalRegistryPathExpected', 'Local registry path expected.');
  end
end
%=============================================================================
function [lock, moduleInfo] = packageMetadata(package_file)
  stagingPath = [usermodulesdir(), '.nmm-tmp/', createGUID(), '/'];
  if ~isdir([usermodulesdir(), '.nmm-tmp/'])
    mkdir([usermodulesdir(), '.nmm-tmp/']);
  end
  mkdir(stagingPath);
  try
    unzip(package_file, stagingPath);
    lockFile = [stagingPath, 'module-lock.json'];
    if ~isfile(lockFile)
      nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
    end
    moduleJson = [stagingPath, 'module.json'];
    if ~isfile(moduleJson)
      nmm_error('ModuleJsonMissing', 'module.json is missing.');
    end
    lock = jsondecode(lockFile, '-file');
    moduleInfo = jsondecode(moduleJson, '-file');
    rmdir(stagingPath, 's');
  catch e
    if isdir(stagingPath)
      rmdir(stagingPath, 's');
    end
    rethrow(e);
  end
end
%=============================================================================
function registry = readLocalRegistry(registryFile)
  if isfile(registryFile)
    verifyRegistryChecksum(registryFile);
    nmm_registry_signature('verify-if-present', registryFile);
    registry = jsondecode(registryFile, '-file');
  else
    registry = struct();
    registry.packages = struct([]);
  end
  if ~isfield(registry, 'packages')
    registry.packages = struct([]);
  end
  % entries with optional fields decode as a cell array: homogenize them
  registry.packages = nmm_normalize_packages(registry.packages);
end
%=============================================================================
function verifyRegistryChecksum(registryFile)
  checksumFile = [registryFile, '.sha256'];
  if ~isfile(checksumFile)
    nmm_error('RegistryChecksumMissing', 'Registry checksum file is missing.');
  end
  expected = strtrim(fileread(checksumFile));
  separator = strfind(expected, ' ');
  if ~isempty(separator)
    expected = expected(1:separator(1) - 1);
  end
  actual = sha256(registryFile, '-file');
  if ~strcmpi(expected, actual)
    nmm_error('RegistryChecksumMismatch', 'Registry checksum does not match.');
  end
end
%=============================================================================
function writeRegistryChecksum(registryFile)
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
end
%=============================================================================
function entry = packageEntry(lock, moduleInfo, package_file, sourceOverride)
  entry = struct();
  entry.name = lock.module;
  entry.version = lock.version;
  entry.title = metadataText(moduleInfo, 'title');
  entry.summary = metadataText(moduleInfo, 'summary');
  entry.description = metadataText(moduleInfo, 'description');
  if isempty(entry.description)
    entry.description = entry.summary;
  end
  entry.license = metadataText(moduleInfo, 'license');
  entry.authors = metadataValue(moduleInfo, 'authors', {});
  entry.repository = metadataText(moduleInfo, 'repository');
  entry.keywords = metadataValue(moduleInfo, 'keywords', {});
  entry.tags = entry.keywords;
  if isfield(lock, 'package_type')
    entry.package_type = lock.package_type;
  else
    entry.package_type = packageType(moduleInfo);
  end
  entry.platforms = {lock.platform};
  entry.nelson = ['>=', lock.nelson.version];
  entry.dependencies = lock.dependencies;
  checksum = sha256(package_file, '-file');
  if ~isempty(sourceOverride)
    source = sourceOverride;
  else
    source = fullpath(package_file);
  end
  if isArchitectureSpecific(lock.platform)
    % A binary (builtin) package is locked to one architecture, so its archive
    % is stored under an "artifacts" map keyed by that architecture. Publishing
    % another architecture of the same version later merges into this map (see
    % upsertPackage), letting one name and version ship win64 and woa64.
    entry.artifacts = struct();
    entry.artifacts.(lock.platform) = archArtifact(source, checksum);
  else
    entry.checksum = checksum;
    entry.source = source;
  end
  entry.signature = '';
end
%=============================================================================
function tf = isArchitectureSpecific(platform)
  tf = ischar(platform) && ~isempty(platform) && ~strcmp(platform, 'all');
end
%=============================================================================
function validateSourceUrl(url, platform, packageVersion)
  % Catch a copy-paste mistake where the hosted -source URL points at a
  % different architecture than the archive being published (e.g. publishing
  % the woa64 .nmz but pasting the win64 release link). The architecture token
  % in the URL file name, when present, must match the packed platform.
  base = lower(urlBasename(url));
  tokens = urlArchTokens(base);
  for candidate = nmm_valid_platforms()
    arch = candidate{1};
    if strcmp(arch, 'all')
      continue
    end
    if any(strcmp(tokens, arch)) && ~strcmp(arch, platform)
      nmm_error('RegistrySourceUrlArchMismatch', 'The -source URL references architecture %s but the package is %s.', arch, platform);
    end
  end
  if ischar(packageVersion) && ~isempty(packageVersion) && isempty(strfind(base, lower(packageVersion)))
    warning('Nelson:nmm:RegistrySourceUrlVersion', 'The -source URL does not mention the package version %s.', packageVersion);
  end
end
%=============================================================================
function base = urlBasename(url)
  positions = strfind(url, '/');
  if isempty(positions)
    base = url;
  else
    base = url(positions(end) + 1:end);
  end
end
%=============================================================================
function tokens = urlArchTokens(base)
  % Whole-token split so 'glnxa64arm' is not matched as the substring 'glnxa64'.
  tokens = strsplit(base, {'-', '.', '_', '/'});
end
%=============================================================================
function artifact = archArtifact(source, checksum)
  artifact = struct();
  artifact.source = source;
  artifact.checksum = checksum;
end
%=============================================================================
function type = packageType(moduleInfo)
  if moduleInfo.builtin
    type = 'binary';
  else
    type = 'source';
  end
end
%=============================================================================
function text = metadataText(moduleInfo, field)
  text = '';
  if isfield(moduleInfo, field)
    value = moduleInfo.(field);
    if ischar(value)
      text = value;
    end
  end
end
%=============================================================================
function value = metadataValue(moduleInfo, field, defaultValue)
  value = defaultValue;
  if isfield(moduleInfo, field)
    value = moduleInfo.(field);
  end
end
%=============================================================================
function packages = upsertPackage(packages, entry)
  if isempty(packages)
    packages = entry;
    return
  end
  index = findPackageIndex(packages, entry.name, entry.version);
  if index == 0
    [packages, entry] = normalizePackageFields(packages, entry);
    packages(end + 1) = entry;
    return
  end
  merged = mergePackageEntry(packages(index), entry);
  [packages, merged] = normalizePackageFields(packages, merged);
  packages(index) = merged;
end
%=============================================================================
function index = findPackageIndex(packages, name, version)
  index = 0;
  for k = 1:length(packages)
    if strcmp(packages(k).name, name) && strcmp(packages(k).version, version)
      index = k;
      return
    end
  end
end
%=============================================================================
function merged = mergePackageEntry(existing, entry)
  % The freshly published metadata wins, but when both the stored entry and the
  % new one carry per-architecture artifacts, accumulate them so a single
  % version keeps every architecture published so far (e.g. win64 then woa64).
  merged = entry;
  if ~isArtifactEntry(entry) || ~isArtifactEntry(existing)
    return
  end
  artifacts = existing.artifacts;
  for field = fieldnames(entry.artifacts)'
    artifacts.(field{1}) = entry.artifacts.(field{1});
  end
  merged.artifacts = artifacts;
  merged.platforms = mergedPlatforms(existing, entry);
end
%=============================================================================
function platforms = mergedPlatforms(existing, entry)
  platforms = {};
  platforms = appendPlatformList(platforms, existing);
  platforms = appendPlatformList(platforms, entry);
end
%=============================================================================
function platforms = appendPlatformList(platforms, package)
  if ~isfield(package, 'platforms')
    return
  end
  values = package.platforms;
  if ischar(values)
    values = {values};
  end
  if ~iscell(values)
    return
  end
  for k = 1:numel(values)
    if ischar(values{k}) && ~isempty(values{k}) && ~any(strcmp(platforms, values{k}))
      platforms{end + 1} = values{k};
    end
  end
end
%=============================================================================
function tf = entryAlreadyPublished(packages, entry)
  % A version is a duplicate when it is already in the registry, except that a
  % new architecture added to an existing binary version is an addition (it
  % merges), not a duplicate: only re-publishing the same architecture needs
  % -force.
  tf = false;
  index = findPackageIndex(packages, entry.name, entry.version);
  if index == 0
    return
  end
  if isArtifactEntry(entry)
    tf = entryHasArtifactArch(packages(index), artifactArch(entry));
    return
  end
  tf = true;
end
%=============================================================================
function tf = isArtifactEntry(entry)
  tf = isfield(entry, 'artifacts') && isstruct(entry.artifacts) ...
      && ~isempty(fieldnames(entry.artifacts));
end
%=============================================================================
function arch = artifactArch(entry)
  names = fieldnames(entry.artifacts);
  arch = names{1};
end
%=============================================================================
function tf = entryHasArtifactArch(existing, arch)
  tf = isfield(existing, 'artifacts') && isstruct(existing.artifacts) ...
      && isfield(existing.artifacts, arch);
end
%=============================================================================
function [packages, entry] = normalizePackageFields(packages, entry)
  for field = fieldnames(entry)'
    if ~isfield(packages, field{1})
      for k = 1:length(packages)
        packages(k).(field{1}) = defaultFieldValue(entry.(field{1}));
      end
    end
  end
  for field = fieldnames(packages)'
    if ~isfield(entry, field{1})
      entry.(field{1}) = defaultFieldValue(packages(1).(field{1}));
    end
  end
end
%=============================================================================
function value = defaultFieldValue(reference)
  if iscell(reference)
    value = {};
  elseif isstruct(reference)
    value = struct();
  elseif islogical(reference)
    value = false;
  else
    value = '';
  end
end
%=============================================================================
