%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = nmm_registry(action, varargin)
  switch action
    case 'search'
      out = registrySearch(varargin{:});
    case 'info'
      out = registryInfo(varargin{:});
    case 'versions'
      out = registryVersions(varargin{:});
    case 'check'
      out = registryCheck(varargin{:});
    case 'validate'
      out = registryCheck(varargin{:});
    case 'list'
      out = registryList(varargin{:});
    case 'stats'
      out = registryStatsCommand(varargin{:});
    case 'doctor'
      out = registryDoctor(varargin{:});
    otherwise
      nmm_error('UnknownRegistryAction', 'Unknown registry action.');
  end
end
%=============================================================================
function result = registrySearch(query, varargin)
  if nargin < 1 || nargin > 3
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if ~ischar(query)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  [asJson, platformOnly] = parseSearchOptions(varargin);
  packages = registryPackages();
  result = struct([]);
  for k = 1:length(packages)
    if ~packageMatches(packages(k), query) || isYankedPackage(packages(k))
      continue
    end
    if platformOnly && ~packageSupportsCurrentPlatform(packages(k))
      continue
    end
    result = appendPackage(result, packages(k));
  end
  if asJson
    result = jsonprettyprint(jsonencode(result));
  end
end
%=============================================================================
function [asJson, platformOnly] = parseSearchOptions(args)
  % -json returns the matches as JSON text; -platform keeps only the packages
  % installable on the running architecture (platforms contains computer('arch')
  % or 'all'). The core search stays unfiltered by default so it reports the
  % whole registry regardless of the current machine; presentation layers such
  % as nmm_gui opt in to -platform.
  asJson = false;
  platformOnly = false;
  for k = 1:numel(args)
    option = args{k};
    if ~ischar(option)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
    end
    switch option
      case '-json'
        asJson = true;
      case '-platform'
        platformOnly = true;
      otherwise
        nmm_error('InvalidOption', '#%d argument: %s expected.', k + 2, '''-json'' or ''-platform''');
    end
  end
end
%=============================================================================
function tf = packageSupportsCurrentPlatform(package)
  platforms = platformList(package);
  if isempty(platforms)
    tf = true;
    return
  end
  arch = computer('arch');
  tf = any(strcmp(platforms, 'all')) || any(strcmp(platforms, arch));
end
%=============================================================================
function result = registryInfo(packageName, varargin)
  if nargin < 1 || nargin > 3
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  version = '';
  asJson = false;
  for k = 1:length(varargin)
    value = varargin{k};
    if ~ischar(value)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 2);
    end
    if strcmp(value, '-json')
      asJson = true;
    else
      if ~isempty(version)
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      version = value;
    end
  end
  packages = registryPackages();
  matches = struct([]);
  for k = 1:length(packages)
    if strcmp(packages(k).name, packageName)
      % a yanked version is never picked as "latest" nor by a semver
      % constraint, but stays resolvable when its exact version is requested
      % (lockfiles must keep working)
      if (isempty(version) || isVersionConstraint(version)) && isYankedPackage(packages(k))
        continue
      end
      if isempty(version) || registryVersionMatches(packages(k).version, version)
        matches = appendPackage(matches, packages(k));
      end
    end
  end
  if isempty(matches)
    nmm_error('PackageNotFound', 'Package not found: %s', packageName);
  end
  result = latestPackage(matches);
  result = resolveArtifactSource(result);
  if asJson
    result = jsonprettyprint(jsonencode(result));
  end
end
%=============================================================================
function package = resolveArtifactSource(package)
  % A binary package can ship one archive per architecture under "artifacts":
  % {"win64": {"source": ..., "checksum": ...}, "woa64": {...}}. Flatten the
  % entry for the running architecture into the flat source/checksum fields the
  % installer already understands, so a single registry entry (one name and
  % version) serves every architecture. Source packages carry no "artifacts"
  % map and are left untouched.
  if ~hasArtifacts(package)
    return
  end
  arch = computer('arch');
  if ~isfield(package.artifacts, arch)
    return
  end
  selected = package.artifacts.(arch);
  if ~isstruct(selected)
    return
  end
  if isfield(selected, 'source') && ischar(selected.source)
    package.source = selected.source;
  end
  if isfield(selected, 'checksum') && ischar(selected.checksum)
    package.checksum = selected.checksum;
  end
end
%=============================================================================
function tf = hasArtifacts(package)
  tf = isfield(package, 'artifacts') && isstruct(package.artifacts) ...
      && ~isempty(fieldnames(package.artifacts));
end
%=============================================================================
function asJson = parseJsonOption(args)
  asJson = false;
  if isempty(args)
    return
  end
  option = args{1};
  if ~ischar(option)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if ~strcmp(option, '-json')
    nmm_error('InvalidOption', '#%d argument: %s expected.', 3, '''-json''');
  end
  asJson = true;
end
%=============================================================================
function tf = registryVersionMatches(packageVersion, requestedVersion)
  tf = strcmp(packageVersion, requestedVersion);
  if tf
    return
  end
  if ~isVersionConstraint(requestedVersion)
    return
  end
  tf = semverConstraintSatisfied(packageVersion, requestedVersion);
end
%=============================================================================
function tf = isVersionConstraint(version)
  tf = startsWith(version, '>') || startsWith(version, '<') ...
      || startsWith(version, '=') || startsWith(version, '^') ...
      || startsWith(version, '~') || contains(version, ' ');
end
%=============================================================================
function tf = semverConstraintSatisfied(packageVersion, constraint)
  tf = true;
  constraints = strsplit(strtrim(constraint), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    try
      if semver(packageVersion, item) ~= 1
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
function versions = registryVersions(packageName, varargin)
  if nargin < 1 || nargin > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  asJson = parseJsonOption(varargin);
  packages = registryPackages();
  versions = {};
  for k = 1:length(packages)
    if strcmp(packages(k).name, packageName) && ~isYankedPackage(packages(k))
      versions{end + 1} = packages(k).version;
    end
  end
  if isempty(versions)
    nmm_error('PackageNotFound', 'Package not found: %s', packageName);
  end
  if asJson
    versions = jsonprettyprint(jsonencode(versions));
  end
end
%=============================================================================
function report = registryCheck(varargin)
  if nargin ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  source = registrySource();
  report = struct();
  report.ok = true;
  report.registry = source;
  report.is_local = ~nmm_is_remote_registry(source);
  report.checksum_file = [source, '.sha256'];
  report.checksum_ok = false;
  report.signature_file = [source, '.sig'];
  report.signature_ok = false;
  report.signature_key = '';
  report.trusted_keys = length(nmm_registry_signature('trusted-keys'));
  report.offline = false;
  report.cached_file = '';
  report.issues = {};
  if ~report.is_local
    try
      [localFile, info] = nmm_registry_fetch(source);
      report.cached_file = localFile;
      report.offline = info.offline;
      report.signature_ok = info.verified;
      report.signature_key = info.public_key;
      if ~info.signature_present
        report = addIssue(report, _('Registry signature file is missing.'));
      end
      jsondecode(localFile, '-file');
    catch
      e = lasterror();
      report = addIssue(report, e.message);
    end
    return
  end
  if ~isfile(source)
    report = addIssue(report, registryNotConfiguredMessage());
    return
  end
  try
    jsondecode(source, '-file');
  catch
    e = lasterror();
    report = addIssue(report, e.message);
  end
  if ~isfile(report.checksum_file)
    report = addIssue(report, _('Registry checksum file is missing.'));
  else
    try
      verifyRegistryChecksum(source);
      report.checksum_ok = true;
    catch
      e = lasterror();
      report = addIssue(report, e.message);
    end
  end
  if isfile(report.signature_file)
    try
      info = nmm_registry_signature('verify', source);
      report.signature_ok = true;
      report.signature_key = info.public_key;
    catch
      e = lasterror();
      report = addIssue(report, e.message);
    end
  end
end
%=============================================================================
function report = registryList(varargin)
  if nargin ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packages = registryPackages();
  report = struct();
  report.registry = registrySource();
  report.packages = packages;
  report.stats = registryStats(packages);
end
%=============================================================================
function report = registryStatsCommand(varargin)
  if nargin ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packages = registryPackages();
  report = registryStats(packages);
end
%=============================================================================
function report = registryDoctor(varargin)
  if nargin ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  report = registryCheck();
  [packages, readIssues] = rawRegistryPackages();
  report.stats = registryStats(packages);
  report.duplicates = duplicatePackages(packages);
  report.missing_sources = missingSources(packages);
  report.inconsistent_artifacts = inconsistentArtifacts(packages);
  report.unsigned = unsignedPackages(packages);
  report.unchecksummed = unchecksummedPackages(packages);
  for k = 1:length(readIssues)
    report = addIssue(report, readIssues{k});
  end
  for k = 1:length(report.duplicates)
    report = addIssue(report, ['Duplicate package version: ', report.duplicates{k}]);
  end
  for k = 1:length(report.missing_sources)
    report = addIssue(report, ['Package source is missing: ', report.missing_sources{k}]);
  end
  for k = 1:length(report.inconsistent_artifacts)
    report = addIssue(report, ['Package artifacts are inconsistent with platforms: ', report.inconsistent_artifacts{k}]);
  end
end
%=============================================================================
function values = inconsistentArtifacts(packages)
  % A binary entry must keep its "platforms" set equal to the architectures it
  % actually ships under "artifacts", and every artifact key must be a known
  % architecture. Hand-editing the registry easily breaks this (listing woa64
  % but forgetting its artifact, or a mistyped arch), which would let an install
  % pass the platform gate and then fail with an obscure missing-source error.
  values = {};
  for k = 1:length(packages)
    if ~hasArtifacts(packages(k))
      continue
    end
    if ~artifactsConsistent(packages(k))
      values = appendUnique(values, packageLabel(packages(k)));
    end
  end
end
%=============================================================================
function tf = artifactsConsistent(package)
  tf = false;
  archs = fieldnames(package.artifacts)';
  valid = nmm_valid_platforms();
  for a = archs
    if strcmp(a{1}, 'all') || ~any(strcmp(valid, a{1}))
      return
    end
  end
  platforms = platformList(package);
  if numel(platforms) ~= numel(archs)
    return
  end
  for a = archs
    if ~any(strcmp(platforms, a{1}))
      return
    end
  end
  tf = true;
end
%=============================================================================
function platforms = platformList(package)
  platforms = {};
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
function stats = registryStats(packages)
  source = registrySource();
  stats = struct();
  stats.registry = source;
  stats.is_local = ~nmm_is_remote_registry(source);
  stats.entries = length(packages);
  stats.packages = length(uniquePackageNames(packages));
  stats.versions = length(packages);
  stats.platforms = uniquePlatforms(packages);
  stats.checksummed = checksummedCount(packages);
  stats.signed = countTextField(packages, 'signature');
end
%=============================================================================
function [packages, issues] = rawRegistryPackages()
  packages = struct([]);
  issues = {};
  source = registrySource();
  if ~nmm_is_remote_registry(source) && ~isfile(source)
    issues{end + 1} = registryNotConfiguredMessage();
    return
  end
  try
    localFile = nmm_registry_fetch(source);
    registry = jsondecode(localFile, '-file');
    packages = packagesFromRegistry(registry);
  catch e
    issues{end + 1} = e.message;
  end
end
%=============================================================================
function report = addIssue(report, issue)
  report.ok = false;
  report.issues{end + 1} = issue;
end
%=============================================================================
function packages = registryPackages()
  registry = readRegistry();
  packages = packagesFromRegistry(registry);
end
%=============================================================================
function packages = packagesFromRegistry(registry)
  if isfield(registry, 'packages')
    packages = registry.packages;
  else
    packages = registry;
  end
  if isempty(packages)
    packages = struct([]);
  end
  % entries may not share the same fields (optional keys): homogenize them
  packages = nmm_normalize_packages(packages);
end
%=============================================================================
function values = uniquePackageNames(packages)
  values = {};
  for k = 1:length(packages)
    if isfield(packages(k), 'name')
      values = appendUnique(values, packages(k).name);
    end
  end
end
%=============================================================================
function values = uniquePlatforms(packages)
  values = {};
  for k = 1:length(packages)
    if isfield(packages(k), 'platforms')
      platforms = packages(k).platforms;
      if iscell(platforms)
        for j = 1:length(platforms)
          values = appendUnique(values, platforms{j});
        end
      elseif isstring(platforms)
        for j = 1:length(platforms)
          values = appendUnique(values, char(platforms(j)));
        end
      elseif ischar(platforms)
        values = appendUnique(values, platforms);
      end
    end
  end
end
%=============================================================================
function values = duplicatePackages(packages)
  values = {};
  seen = {};
  for k = 1:length(packages)
    if isfield(packages(k), 'name') && isfield(packages(k), 'version')
      label = [packages(k).name, '@', packages(k).version];
      if any(strcmp(seen, label))
        values = appendUnique(values, label);
      else
        seen{end + 1} = label;
      end
    end
  end
end
%=============================================================================
function values = missingSources(packages)
  values = {};
  for k = 1:length(packages)
    if ~packageSourcePresent(packages(k))
      values = appendUnique(values, packageLabel(packages(k)));
    end
  end
end
%=============================================================================
function tf = packageSourcePresent(package)
  if hasArtifacts(package)
    tf = true;
    for field = fieldnames(package.artifacts)'
      selected = package.artifacts.(field{1});
      source = '';
      if isstruct(selected) && isfield(selected, 'source') && ischar(selected.source)
        source = selected.source;
      end
      if ~sourceResolves(source)
        tf = false;
        return
      end
    end
    return
  end
  tf = sourceResolves(packageSource(package));
end
%=============================================================================
function tf = sourceResolves(source)
  tf = ischar(source) && ~isempty(source) ...
      && (isfile(source) || isdir(source) ...
      || startsWith(source, 'http://') || startsWith(source, 'https://'));
end
%=============================================================================
function values = unsignedPackages(packages)
  values = {};
  for k = 1:length(packages)
    if ~hasTextField(packages(k), 'signature')
      values = appendUnique(values, packageLabel(packages(k)));
    end
  end
end
%=============================================================================
function values = unchecksummedPackages(packages)
  values = {};
  for k = 1:length(packages)
    if ~packageChecksummed(packages(k))
      values = appendUnique(values, packageLabel(packages(k)));
    end
  end
end
%=============================================================================
function count = checksummedCount(packages)
  count = 0;
  for k = 1:length(packages)
    if packageChecksummed(packages(k))
      count = count + 1;
    end
  end
end
%=============================================================================
function tf = packageChecksummed(package)
  if hasArtifacts(package)
    tf = true;
    for field = fieldnames(package.artifacts)'
      selected = package.artifacts.(field{1});
      if ~(isstruct(selected) && isfield(selected, 'checksum') ...
          && ischar(selected.checksum) && ~isempty(selected.checksum))
        tf = false;
        return
      end
    end
    return
  end
  tf = hasTextField(package, 'checksum');
end
%=============================================================================
function count = countTextField(packages, field)
  count = 0;
  for k = 1:length(packages)
    if hasTextField(packages(k), field)
      count = count + 1;
    end
  end
end
%=============================================================================
function tf = hasTextField(package, field)
  tf = isfield(package, field) && ischar(package.(field)) && ~isempty(package.(field));
end
%=============================================================================
function source = packageSource(package)
  source = '';
  if isfield(package, 'source')
    source = package.source;
  elseif isfield(package, 'url')
    source = package.url;
  elseif isfield(package, 'archive')
    source = package.archive;
  end
  if ~ischar(source)
    source = '';
  end
end
%=============================================================================
function label = packageLabel(package)
  label = '';
  if isfield(package, 'name')
    label = package.name;
  end
  if isfield(package, 'version')
    label = [label, '@', package.version];
  end
end
%=============================================================================
function values = appendUnique(values, value)
  if ~any(strcmp(values, value))
    values{end + 1} = value;
  end
end
%=============================================================================
function registry = readRegistry()
  % the registry bytes are fetched and verified (checksum, Ed25519
  % signature) by nmm_registry_fetch before being parsed
  source = registrySource();
  if ~nmm_is_remote_registry(source) && ~isfile(source)
    nmm_error('RegistryNotConfigured', registryNotConfiguredMessage());
  end
  localFile = nmm_registry_fetch(source);
  registry = jsondecode(localFile, '-file');
end
%=============================================================================
function msg = registryNotConfiguredMessage()
  msg = 'Package registry is not configured. Set NELSON_NMM_REGISTRY or create registry.json in usermodulesdir(). Local source, archive and offline cache commands remain available.';
end
%=============================================================================
function source = registrySource()
  source = nmm_registry_source();
end
%=============================================================================
function verifyRegistryChecksum(source)
  checksumFile = [source, '.sha256'];
  if ~isfile(checksumFile)
    nmm_error('RegistryChecksumMissing', 'Registry checksum file is missing.');
  end
  expected = strtrim(fileread(checksumFile));
  separator = strfind(expected, ' ');
  if ~isempty(separator)
    expected = expected(1:separator(1) - 1);
  end
  actual = sha256(source, '-file');
  if ~strcmpi(expected, actual)
    nmm_error('RegistryChecksumMismatch', 'Registry checksum does not match.');
  end
end
%=============================================================================
function tf = isYankedPackage(package)
  % "yanked": true marks a published version withdrawn by its author.
  tf = false;
  if isfield(package, 'yanked') && ~isempty(package.yanked)
    value = package.yanked;
    if islogical(value) || isnumeric(value)
      tf = logical(value(1));
    elseif ischar(value)
      tf = any(strcmpi(value, {'true', '1', 'yes'}));
    end
  end
end
%=============================================================================
function tf = packageMatches(package, query)
  q = lower(query);
  text = lower(packageText(package));
  tf = contains(text, q);
end
%=============================================================================
function text = packageText(package)
  text = '';
  fields = {'name', 'version', 'summary', 'description', 'keywords'};
  for field = fields
    if isfield(package, field{1})
      value = package.(field{1});
      if ischar(value)
        text = [text, ' ', value];
      elseif iscellstr(value)
        for k = value(:)'
          text = [text, ' ', k{1}];
        end
      elseif isstring(value)
        text = [text, ' ', char(join(value, ' '))];
      end
    end
  end
end
%=============================================================================
function result = appendPackage(result, package)
  if isempty(result)
    result = package;
  else
    result(end + 1) = package;
  end
end
%=============================================================================
function latest = latestPackage(packages)
  latest = packages(1);
  for k = 2:length(packages)
    if semver(packages(k).version, latest.version) == 1
      latest = packages(k);
    end
  end
end
%=============================================================================
