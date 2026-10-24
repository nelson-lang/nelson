%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function package_file = nmm_pack(varargin)
  if length(varargin) < 2 || length(varargin) > 5
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_path = varargin{1};
  destination_dir = varargin{2};
  skipTests = false;
  dryRun = false;
  target = computer('arch');
  options = varargin(3:end);
  if length(varargin) >= 2 && ischar(destination_dir) && strcmp(destination_dir, '-dry-run')
    dryRun = true;
    destination_dir = '';
  end
  k = 1;
  while k <= numel(options)
    option = options{k};
    if ~ischar(option)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 3);
    end
    if strcmp(option, '-no-tests')
      skipTests = true;
    elseif strcmp(option, '-dry-run')
      dryRun = true;
    elseif strcmp(option, '-target') && k < numel(options) && ischar(options{k + 1})
      target = options{k + 1};
      k = k + 1;
    else
      nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-no-tests'', ''-dry-run'' or ''-target'' <platform>', k + 3));
    end
    k = k + 1;
  end
  % A cross build only exists for the WebAssembly engine: the dynamic-link
  % tools then produce an Emscripten side module (see dltarget).
  crossBuild = ~strcmp(target, computer('arch'));
  if crossBuild && ~strcmp(target, 'wasm32')
    nmm_error('UnsupportedPackTarget', 'Unsupported pack target: %s', target);
  end
  if ~ischar(module_path)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(destination_dir)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if ~dryRun && ~isdir(destination_dir)
    nmm_error('DirectoryExpectedExistingDirectoryExpected', 'an existing directory expected.');
  end
  module_path = ensureTrailingSlash(module_path);
  nmm_validate(module_path, '-target', target);
  info = nmm_read_module_json(module_path);
  stagingPath = newTemporaryDirectory();
  try
    copyfile(module_path, stagingPath, 'f');
    if crossBuild
      previousTarget = getenv('NELSON_DL_TARGET');
      restoreTarget = onCleanup(@() setenv('NELSON_DL_TARGET', previousTarget));
      setenv('NELSON_DL_TARGET', target);
    end
    % the variable stays set until the end: the lock step reruns the builder
    run([stagingPath, 'builder.m']);
    % a gateway built for another architecture cannot run here; its tests are
    % still packaged for the installing engine
    testsRun = ~skipTests && ~crossBuild;
    if testsRun
      runPackageTests(stagingPath);
    end
    lock = nmm_lock(stagingPath, '-target', target);
    validateBuiltPackageStructure(stagingPath);
    selection = computePackSelection(stagingPath, skipTests);
    if dryRun
      package_file = dryRunReport(info, lock, destination_dir, ~testsRun, selection, target);
      cleanupDirectory(stagingPath);
      return
    end
    applyPackSelection(stagingPath, selection);
    package_file = buildDestinationFilename(destination_dir, info.module, info, target);
    zip(package_file, stagingPath, stagingPath);
    filewrite([package_file, '.sha256'], sha256(package_file, '-file'));
    cleanupDirectory(stagingPath);
  catch
    e = lasterror();
    cleanupDirectory(stagingPath);
    error(e);
  end
end
%=============================================================================
function report = dryRunReport(info, lock, destination_dir, skipTests, selection, target)
  report = struct();
  report.ok = true;
  report.dry_run = true;
  report.module = info.module;
  report.version = info.version;
  report.tests_run = ~skipTests;
  report.lock = lock;
  report.files = selection(:)';
  report.package_file = '';
  if ~isempty(destination_dir)
    report.package_file = buildDestinationFilename(destination_dir, info.module, info, target);
  end
  printPackSelection(info.module, selection);
end
%=============================================================================
function printPackSelection(module_name, selection)
  fprintf(_('Files to be packaged for module ''%s'' (%d):\n'), module_name, numel(selection));
  for k = 1:numel(selection)
    fprintf('  %s\n', selection{k});
  end
end
%=============================================================================
% Computes the relative paths (posix '/' separators) that will remain in the
% .nmz archive. The selection reads its rules (.nmignore, module.json 'files'
% allowlist) and walks the built staging tree, so generated artifacts are
% considered while default excludes drop build junk (see nmm_pack_select).
% Files nmm needs at install time are always kept regardless of the rules.
function selection = computePackSelection(stagingPath, skipTests)
  options = struct();
  options.skiptests = skipTests;
  selection = nmm_pack_select(stagingPath, options);
  essentials = essentialInstallFiles(stagingPath);
  for k = 1:numel(essentials)
    if ~any(strcmp(selection, essentials{k}))
      selection{end + 1} = essentials{k};
    end
  end
  if ~isempty(selection)
    selection = sort(selection(:))';
  end
end
%=============================================================================
% Files that must ship for nmm to load/verify a package, kept even when a
% 'files' allowlist or .nmignore would otherwise drop them.
function files = essentialInstallFiles(stagingPath)
  root = ensureTrailingSlash(stagingPath);
  candidates = {'module.json', 'module-lock.json', 'loader.m'};
  files = {};
  for k = 1:numel(candidates)
    if isfile([root, candidates{k}])
      files{end + 1} = candidates{k};
    end
  end
  % Locale catalogs (locale/<module>-ui-<locale>.json,
  % locale/<module>-errors-<locale>.json) ship whenever present, so a module's
  % translations reach installs regardless of a 'files' allowlist or .nmignore.
  if isdir([root, 'locale'])
    localeEntries = dir([root, 'locale/*.json']);
    for k = 1:numel(localeEntries)
      if ~localeEntries(k).isdir
        files{end + 1} = ['locale/', localeEntries(k).name]; %#ok<AGROW>
      end
    end
  end
end
%=============================================================================
% Removes from the staging tree every file that is not part of the selection,
% then prunes the directories left empty. The .nmz is zipped from what remains.
function applyPackSelection(stagingPath, selection)
  root = ensureTrailingSlash(stagingPath);
  allFiles = listAllRelativeFiles(root, '');
  for k = 1:numel(allFiles)
    if ~any(strcmp(selection, allFiles{k}))
      target = [root, allFiles{k}];
      if isfile(target)
        rmfile(target);
      end
    end
  end
  removeEmptyDirectories(root);
end
%=============================================================================
function files = listAllRelativeFiles(root, rel)
  files = {};
  if isempty(rel)
    listing = dir(root);
  else
    listing = dir([root, rel]);
  end
  for i = 1:numel(listing)
    nm = listing(i).name;
    if strcmp(nm, '.') || strcmp(nm, '..')
      continue;
    end
    if isempty(rel)
      childRel = nm;
    else
      childRel = [rel, '/', nm];
    end
    if listing(i).isdir
      files = [files, listAllRelativeFiles(root, childRel)];
    else
      files{end + 1} = childRel;
    end
  end
end
%=============================================================================
function removeEmptyDirectories(root)
  changed = true;
  while changed
    changed = false;
    dirs = listAllRelativeDirs(root, '');
    % deepest first so a parent becomes empty after its children are removed
    dirs = sort(dirs(:))';
    for k = numel(dirs):-1:1
      full = [root, dirs{k}];
      entries = dir(full);
      isEmpty = true;
      for j = 1:numel(entries)
        if ~strcmp(entries(j).name, '.') && ~strcmp(entries(j).name, '..')
          isEmpty = false;
          break;
        end
      end
      if isEmpty
        rmdir(full);
        changed = true;
      end
    end
  end
end
%=============================================================================
function dirs = listAllRelativeDirs(root, rel)
  dirs = {};
  if isempty(rel)
    listing = dir(root);
  else
    listing = dir([root, rel]);
  end
  for i = 1:numel(listing)
    nm = listing(i).name;
    if strcmp(nm, '.') || strcmp(nm, '..')
      continue;
    end
    if ~listing(i).isdir
      continue;
    end
    if isempty(rel)
      childRel = nm;
    else
      childRel = [rel, '/', nm];
    end
    dirs{end + 1} = childRel;
    dirs = [dirs, listAllRelativeDirs(root, childRel)];
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
function validateBuiltPackageStructure(module_path)
  if ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  if ~isfile([module_path, 'module-lock.json'])
    nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
  end
end
%=============================================================================
function package_file = buildDestinationFilename(destination_dir, module_name, info, target)
  if info.builtin
    arch = target;
  else
    arch = 'all';
  end
  if endsWith(fullpath(destination_dir), '/')
    package_file = sprintf('%s%s-%s-%s.nmz', fullpath(destination_dir), module_name, arch, info.version);
  else
    package_file = sprintf('%s/%s-%s-%s.nmz', fullpath(destination_dir), module_name, arch, info.version);
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
