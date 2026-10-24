%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function package_file = nmm_package(varargin)
  if (length(varargin) ~= 2)
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_name = varargin{1};
  destination_dir = varargin{2};
  package_file = [];
  if ~nmm_is_installed(module_name)
    nmm_error('ModuleNameExpectedValidModuleName', 'an valid module name expected.');
  end
  if ~isdir(destination_dir)
    nmm_error('DirectoryExpectedExistingDirectoryExpected', 'an existing directory expected.');
  end
  modules = nmm_list();
  module_info = getfield(modules, module_name);
  module_path = module_info.path;
  stagingPath = newTemporaryDirectory();
  try
    copyfile(module_path, stagingPath, 'f');
    ensureLockFileHasChecksums(stagingPath);
    info = nmm_read_module_json(stagingPath);
    package_file = buildDestinationFilename(destination_dir, module_name, info);
    zip(package_file, stagingPath, stagingPath);
    writePackageChecksum(package_file);
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function ensureLockFileHasChecksums(module_path)
  root = ensureTrailingSlash(module_path);
  lockFile = [root, 'module-lock.json'];
  if ~isfile(lockFile)
    nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
  end
  lock = jsondecode(lockFile, '-file');
  moduleInfo = nmm_read_module_json(root);
  lock.package_type = packageType(moduleInfo);
  lock.checksums = lockChecksums(root);
  filewrite(lockFile, jsonprettyprint(jsonencode(lock)));
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
function package_file = buildDestinationFilename(destination_dir, module_name, info)
  package_file_extension = 'nmz';
  universal = false;
  if info.builtin
    arch = computer('arch');
  else
    platforms = info.platforms;
    if ischar(platforms)
      universal = strcmp(info.platforms, 'all');
    end
  end
  if universal
    arch = 'all';
  else
    arch = computer('arch');
  end
  version = info.version;
  if endsWith(fullpath(destination_dir), '/')
    buffer_format = '%s%s-%s-%s.%s';
  else
    buffer_format = '%s/%s-%s-%s.%s';
  end
  package_file = sprintf(buffer_format, fullpath(destination_dir), module_name, arch, version, package_file_extension);
end
%=============================================================================
function writePackageChecksum(package_file)
  checksum_file = [package_file, '.sha256'];
  filewrite(checksum_file, sha256(package_file, '-file'));
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
