%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lock = nmm_lock(varargin)
  if length(varargin) < 1 || length(varargin) > 4
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_path = varargin{1};
  if ~ischar(module_path)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  option = '';
  arch = computer('arch');
  k = 2;
  while k <= length(varargin)
    argument = varargin{k};
    if ~ischar(argument)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 1);
    end
    if strcmp(argument, '-target') && k < length(varargin) && ischar(varargin{k + 1})
      % architecture the gateway was built for (cross build, see nmm('pack'))
      arch = varargin{k + 1};
      k = k + 1;
    else
      option = argument;
    end
    k = k + 1;
  end
  module_path = ensureTrailingSlash(module_path);
  switch option
    case ''
      lock = writeModuleLock(module_path, arch);
    case '-dry-run'
      lock = computeModuleLock(module_path, arch);
    case {'-check', '-diff'}
      lock = lockReport(module_path, option);
    otherwise
      nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-dry-run'', ''-check'' or ''-diff''', 3));
  end
end
%=============================================================================
function lock = writeModuleLock(module_path, arch)
  nmm_validate(module_path, '-target', arch);
  if isfile([module_path, 'builder.m'])
    run([module_path, 'builder.m']);
  end
  if ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  lock = buildModuleLock(module_path, arch);
  filewrite([module_path, 'module-lock.json'], jsonprettyprint(jsonencode(lock)));
end
%=============================================================================
function lock = computeModuleLock(module_path, arch)
  nmm_validate(module_path, '-target', arch);
  stagingPath = newTemporaryDirectory();
  try
    copyfile(module_path, stagingPath, 'f');
    if isfile([stagingPath, 'builder.m'])
      run([stagingPath, 'builder.m']);
    end
    if ~isfile([stagingPath, 'loader.m'])
      nmm_error('LoaderMissing', 'loader.m is missing.');
    end
    lock = buildModuleLock(stagingPath, arch);
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function report = lockReport(module_path, option)
  expected = computeModuleLock(module_path, computer('arch'));
  current = readCurrentLock(module_path);
  differences = lockDifferences(current, expected);
  report = struct();
  report.ok = isempty(differences);
  report.up_to_date = report.ok;
  report.module = expected.module;
  report.version = expected.version;
  report.lockfile = [module_path, 'module-lock.json'];
  report.differences = differences;
  if strcmp(option, '-diff')
    report.current = current;
    report.expected = expected;
  end
end
%=============================================================================
function current = readCurrentLock(module_path)
  lockfile = [module_path, 'module-lock.json'];
  current = struct([]);
  if isfile(lockfile)
    current = jsondecode(lockfile, '-file');
  end
end
%=============================================================================
function differences = lockDifferences(current, expected)
  differences = {};
  if isempty(current)
    differences{end + 1} = _('module-lock.json is missing.');
    return
  end
  fields = unionFields(fieldnames(current), fieldnames(expected));
  for k = 1:length(fields)
    name = fields{k};
    if ~isfield(current, name)
      differences{end + 1} = ['missing field: ', name];
    elseif ~isfield(expected, name)
      differences{end + 1} = ['unexpected field: ', name];
    elseif ~strcmp(jsonencode(current.(name)), jsonencode(expected.(name)))
      differences{end + 1} = ['changed field: ', name];
    end
  end
end
%=============================================================================
function fields = unionFields(left, right)
  fields = left(:)';
  for k = 1:length(right)
    if ~any(strcmp(fields, right{k}))
      fields{end + 1} = right{k};
    end
  end
end
%=============================================================================
function lock = buildModuleLock(module_path, arch)
  info = nmm_read_module_json(module_path);
  lock.module = info.module;
  lock.version = info.version;
  lock.lock = 2;
  lock.lock_format = 'nelson.nmm.lock/2';
  if info.builtin
    lock.platform = arch;
  else
    lock.platform = 'all';
  end
  lock.builtin = info.builtin;
  lock.package_type = packageType(info);
  source = struct();
  source.type = 'source';
  source.reference = '';
  lock.source = source;
  nelson = struct();
  nelson.version = nelsonVersionString();
  nelson.arch = arch;
  nelson.abi = nelsonAbiTag(arch);
  lock.nelson = nelson;
  lock.checksums = lockChecksums(module_path);
  lock.dependencies = lockedDependencies(info);
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
function dependencies = lockedDependencies(info)
  dependencies = struct();
  if ~isfield(info, 'dependencies')
    return
  end
  modules = nmm_list();
  for dependency = fieldnames(info.dependencies)'
    name = dependency{1};
    if ~isfield(modules, name)
      nmm_error('DependencyNotInstalled', '%s not installed.', name);
    end
    moduleInfo = getfield(modules, name);
    dependencies.(name) = moduleInfo.version;
  end
end
%=============================================================================
function checksums = lockChecksums(module_path)
  root = ensureTrailingSlash(module_path);
  checksums = struct();
  checksums = addFileChecksum(checksums, 'module_json', [root, 'module.json']);
  checksums = addFileChecksum(checksums, 'loader', [root, 'loader.m']);
  % builder.m is a build-only script excluded from the package (see
  % nmm_pack_default_excludes), so it is not part of the shipped checksums.
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
function s = nelsonVersionString()
  v = version('-number');
  s = sprintf('%d.%d.%d', v(1), v(2), v(3));
end
%=============================================================================
function tag = nelsonAbiTag(arch)
  if strcmp(arch, 'wasm32')
    % the WebAssembly engine is 32-bit; this is what version('-compiler')
    % reports there, so a cross-built package verifies on install.
    tag = 'wasm32-32';
    return
  end
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
