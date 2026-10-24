%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_verify(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  target = varargin{1};
  if ~ischar(target)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if isdir(target)
    report = verifySourceModule(target);
    return
  end
  if ~isfile(target)
    nmm_error('FilenameExpected', 'filename expected.');
  end
  if ~endsWith(target, '.nmz')
    nmm_error('InvalidPackageFileExtension', 'filename extension .nmz expected.');
  end
  verifyPackageChecksum(target);
  stagingPath = newTemporaryDirectory();
  try
    unzip(target, stagingPath);
    nmm_validate(stagingPath);
    validateBuiltPackageStructure(stagingPath);
    moduleInfo = nmm_read_module_json(stagingPath);
    lock = jsondecode([stagingPath, 'module-lock.json'], '-file');
    validateArchiveLock(lock, moduleInfo);
    validateLockChecksums(stagingPath, lock);
    report = verificationReport(target, lock);
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function report = verifySourceModule(module_path)
  module_path = ensureTrailingSlash(module_path);
  check = nmm_lock(module_path, '-check');
  if ~check.ok
    nmm_error('ModuleLockOutOfDate', 'module-lock.json is not up to date.');
  end
  report = struct();
  report.ok = true;
  report.package = module_path;
  report.module = check.module;
  report.version = check.version;
  report.platform = '';
  report.package_type = 'source';
  report.has_sha256 = false;
  report.lockfile = check.lockfile;
  report.up_to_date = true;
  report.differences = {};
end
%=============================================================================
function report = verificationReport(package_file, lock)
  report = struct();
  report.ok = true;
  report.package = package_file;
  report.module = lock.module;
  report.version = lock.version;
  report.platform = lock.platform;
  report.package_type = lock.package_type;
  report.has_sha256 = isfile([package_file, '.sha256']);
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
function validateBuiltPackageStructure(module_path)
  if ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  if ~isfile([module_path, 'module-lock.json'])
    nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
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
function validateLockChecksums(modulePath, lock)
  if ~isfield(lock, 'checksums') || ~isstruct(lock.checksums)
    nmm_error('ModuleLockChecksumsMissing', 'module-lock.json field is missing: checksums');
  end
  checksums = lock.checksums;
  for field = {'module_json', 'loader'}
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
function tag = nelsonAbiTag()
  compiler = version('-compiler');
  tag = [computer('arch'), '-', compiler{3}];
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
    rmdir(path, 's');
  end
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
