%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = nmm_cache(varargin)
  if length(varargin) < 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  action = varargin{1};
  if ~ischar(action)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  switch action
    case 'dir'
      if length(varargin) ~= 1
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = cacheRoot();
    case 'clear'
      if length(varargin) ~= 1
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      root = cacheRoot();
      if isdir(root)
        rmdir(root, 's');
      end
      mkdir(root);
      out = true;
    case 'list'
      if length(varargin) ~= 1
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = listCachedPackages();
    case 'size'
      if length(varargin) ~= 1
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = cacheSize();
    case 'remove'
      if length(varargin) ~= 3
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = removeNamedCachedPackage(varargin{2}, varargin{3});
    case 'info'
      if length(varargin) ~= 3
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = cacheInfo(varargin{2}, varargin{3});
    case 'has'
      if length(varargin) ~= 3
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = cacheHas(varargin{2}, varargin{3});
    case 'add'
      if length(varargin) ~= 2
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = addCachedPackage(varargin{2});
    case 'export'
      if length(varargin) ~= 2
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = exportCache(varargin{2});
    case 'import'
      if length(varargin) ~= 2
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = importCache(varargin{2});
    case 'prune'
      if length(varargin) > 2
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      dryRun = false;
      if length(varargin) == 2
        if ~strcmp(varargin{2}, '-dry-run')
          nmm_error('InvalidArgumentValue', 'Wrong value for input argument.');
        end
        dryRun = true;
      end
      out = pruneCache(dryRun);
    case 'verify'
      if length(varargin) ~= 1
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = verifyCache();
    case 'store'
      if length(varargin) ~= 4
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = storePackage(varargin{2}, varargin{3}, varargin{4});
    case 'package'
      if length(varargin) ~= 3
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      out = cachedPackage(varargin{2}, varargin{3});
    case 'latest'
      if length(varargin) < 2 || length(varargin) > 3
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      if length(varargin) == 2
        out = latestCachedPackage(varargin{2}, '');
      else
        out = latestCachedPackage(varargin{2}, varargin{3});
      end
    otherwise
      nmm_error('UnknownCacheAction', 'Unknown cache action.');
  end
end
%=============================================================================
function report = cacheInfo(moduleName, moduleVersion)
  packageFile = cachedPackage(moduleName, moduleVersion);
  checksumFile = [packageFile, '.sha256'];
  report = struct();
  report.name = moduleName;
  report.version = moduleVersion;
  report.path = packageFile;
  report.checksum_file = checksumFile;
  report.exists = isfile(packageFile);
  report.has_sha256 = isfile(checksumFile);
  report.checksum = '';
  report.checksum_ok = false;
  report.bytes = fileSize(packageFile);
  report.date = '';
  info = dir(packageFile);
  if ~isempty(info) && isfield(info, 'date')
    report.date = info(1).date;
  end
  if ~report.exists
    return
  end
  report.checksum = sha256(packageFile, '-file');
  if report.has_sha256
    report.checksum_ok = strcmpi(readChecksum(checksumFile), report.checksum);
  end
end
%=============================================================================
function checksum = readChecksum(checksumFile)
  checksum = strtrim(fileread(checksumFile));
  separator = strfind(checksum, ' ');
  if ~isempty(separator)
    checksum = checksum(1:separator(1) - 1);
  end
end
%=============================================================================
function tf = cacheHas(moduleName, moduleVersion)
  tf = isfile(cachedPackage(moduleName, moduleVersion));
end
%=============================================================================
function report = addCachedPackage(packageFile)
  checked = nmm_verify(packageFile);
  destination = storePackage(packageFile, checked.module, checked.version);
  report = checked;
  report.added = true;
  report.source = packageFile;
  report.path = destination;
end
%=============================================================================
function report = exportCache(destinationDir)
  if ~ischar(destinationDir)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  destinationDir = ensureTrailingSlash(destinationDir);
  if ~isdir(destinationDir)
    mkdir(destinationDir);
  end
  packages = listCachedPackages();
  copied = {};
  for k = 1:length(packages)
    copied = copyCacheFile(packages(k).path, destinationDir, copied);
    checksumFile = [packages(k).path, '.sha256'];
    if isfile(checksumFile)
      copied = copyCacheFile(checksumFile, destinationDir, copied);
    end
  end
  report = struct();
  report.destination = destinationDir;
  report.files = copied;
  report.packages = packages;
end
%=============================================================================
function copied = copyCacheFile(sourceFile, destinationDir, copied)
  [~, name, extension] = fileparts(sourceFile);
  destinationFile = [destinationDir, name, extension];
  copyfile(sourceFile, destinationFile, 'f');
  copied{end + 1} = destinationFile;
end
%=============================================================================
function report = importCache(sourceDir)
  if ~ischar(sourceDir)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  sourceDir = ensureTrailingSlash(sourceDir);
  if ~isdir(sourceDir)
    nmm_error('DirectoryExpected', 'directory expected.');
  end
  files = dir([sourceDir, '*.nmz']);
  imported = {};
  packages = struct([]);
  for k = 1:length(files)
    packageFile = [sourceDir, files(k).name];
    item = addCachedPackage(packageFile);
    imported{end + 1} = item.path;
    packages = appendPackage(packages, item);
  end
  report = struct();
  report.source = sourceDir;
  report.files = imported;
  report.packages = packages;
end
%=============================================================================
function packages = appendPackage(packages, item)
  package = struct();
  package.name = item.module;
  package.version = item.version;
  package.path = item.path;
  if isempty(packages)
    packages = package;
  else
    packages(end + 1) = package;
  end
end
%=============================================================================
function report = cacheSize()
  packages = listCachedPackages();
  bytes = 0;
  for k = 1:length(packages)
    bytes = bytes + fileSize(packages(k).path);
    bytes = bytes + fileSize([packages(k).path, '.sha256']);
  end
  report = struct();
  report.archives = length(packages);
  report.bytes = bytes;
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
function report = removeNamedCachedPackage(moduleName, moduleVersion)
  packageFile = cachedPackage(moduleName, moduleVersion);
  existed = isfile(packageFile) || isfile([packageFile, '.sha256']);
  removeCachedPackage(packageFile);
  report = struct();
  report.package = moduleName;
  report.version = moduleVersion;
  report.path = packageFile;
  report.removed = existed;
end
%=============================================================================
function report = pruneCache(dryRun)
  packages = listCachedPackages();
  keep = latestPackagePaths(packages);
  removed = {};
  for k = 1:length(packages)
    if ~any(strcmp(keep, packages(k).path))
      removed{end + 1} = packages(k).path;
      if ~dryRun
        removeCachedPackage(packages(k).path);
      end
    end
  end
  report = struct();
  report.dry_run = dryRun;
  report.removed = removed;
end
%=============================================================================
function keep = latestPackagePaths(packages)
  keep = {};
  names = {};
  for k = 1:length(packages)
    if any(strcmp(names, packages(k).name))
      continue
    end
    names{end + 1} = packages(k).name;
    latest = packages(k);
    for j = 1:length(packages)
      if strcmp(packages(j).name, latest.name) && semver(packages(j).version, latest.version) == 1
        latest = packages(j);
      end
    end
    keep{end + 1} = latest.path;
  end
end
%=============================================================================
function removeCachedPackage(packageFile)
  if isfile(packageFile)
    rmfile(packageFile);
  end
  checksumFile = [packageFile, '.sha256'];
  if isfile(checksumFile)
    rmfile(checksumFile);
  end
end
%=============================================================================
function report = verifyCache()
  packages = listCachedPackages();
  corrupted = struct([]);
  for k = 1:length(packages)
    try
      nmm_verify(packages(k).path);
    catch
      e = lasterror();
      item = packages(k);
      item.error = e.message;
      if isempty(corrupted)
        corrupted = item;
      else
        corrupted(end + 1) = item;
      end
    end
  end
  report = struct();
  report.ok = isempty(corrupted);
  report.corrupted = corrupted;
end
%=============================================================================
function packages = listCachedPackages()
  files = dir([cacheRoot(), '*.nmz']);
  packages = struct([]);
  for k = 1:length(files)
    item = cachedPackageInfo(files(k).name);
    if isempty(item.name)
      continue
    end
    if isempty(packages)
      packages = item;
    else
      packages(end + 1) = item;
    end
  end
end
%=============================================================================
function item = cachedPackageInfo(filename)
  item = struct();
  item.name = '';
  item.version = '';
  item.path = [cacheRoot(), filename];
  item.has_sha256 = isfile([item.path, '.sha256']);
  if ~endsWith(filename, '.nmz')
    return
  end
  basename = filename(1:end - length('.nmz'));
  separator = strfind(basename, '-');
  if isempty(separator)
    return
  end
  last = separator(end);
  item.name = basename(1:last - 1);
  item.version = basename(last + 1:end);
end
%=============================================================================
function root = cacheRoot()
  root = [usermodulesdir(), '.nmm-cache/'];
  if ~isdir(root)
    mkdir(root);
  end
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
function filename = storePackage(packageFile, moduleName, moduleVersion)
  if ~isfile(packageFile)
    nmm_error('FilenameExpected', 'filename expected.');
  end
  checksumFile = [packageFile, '.sha256'];
  filename = cachedPackage(moduleName, moduleVersion);
  copyfile(packageFile, filename, 'f');
  if isfile(checksumFile)
    copyfile(checksumFile, [filename, '.sha256'], 'f');
  end
end
%=============================================================================
function filename = cachedPackage(moduleName, moduleVersion)
  if ~ischar(moduleName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if ~ischar(moduleVersion)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 4);
  end
  filename = [cacheRoot(), moduleName, '-', moduleVersion, '.nmz'];
end
%=============================================================================
function filename = latestCachedPackage(moduleName, versionSpec)
  if ~ischar(moduleName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if ~ischar(versionSpec)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 4);
  end
  files = dir([cacheRoot(), moduleName, '-*.nmz']);
  filename = '';
  selectedVersion = '';
  for k = 1:length(files)
    version = cachedVersion(moduleName, files(k).name);
    if isempty(version) || ~versionMatches(version, versionSpec)
      continue
    end
    if isempty(selectedVersion) || semver(version, selectedVersion) == 1
      selectedVersion = version;
      filename = [cacheRoot(), files(k).name];
    end
  end
  if isempty(filename)
    nmm_error('CachePackageNotFound', 'Package is not available in cache.');
  end
end
%=============================================================================
function version = cachedVersion(moduleName, filename)
  prefix = [moduleName, '-'];
  suffix = '.nmz';
  version = '';
  if startsWith(filename, prefix) && endsWith(filename, suffix)
    version = filename(length(prefix) + 1:end - length(suffix));
  end
end
%=============================================================================
function tf = versionMatches(version, versionSpec)
  if isempty(versionSpec)
    tf = true;
    return
  end
  if isVersionConstraint(versionSpec)
    tf = semverConstraintSatisfied(version, versionSpec);
  else
    tf = strcmp(version, versionSpec);
  end
end
%=============================================================================
function tf = isVersionConstraint(versionSpec)
  tf = startsWith(versionSpec, '>') || startsWith(versionSpec, '<') ...
      || startsWith(versionSpec, '=') || startsWith(versionSpec, '^') ...
      || startsWith(versionSpec, '~') || contains(versionSpec, ' ');
end
%=============================================================================
function tf = semverConstraintSatisfied(version, versionSpec)
  tf = true;
  constraints = strsplit(strtrim(versionSpec), ' ');
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
