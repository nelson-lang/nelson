%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
ncc('--help');
try
check_rebuild();

catch installer_error
  skip_installer_antivirus(installer_error);
end
function check_rebuild()
  work = [tempname(fullfile(nelsonroot(), 'temp')), ' rebuild '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  source = fullfile(work, 'source');
  mkdir(source);
  entry = fullfile(source, 'rebuild_entry.m');
  filewrite(entry, 'function rebuild_entry(); disp(''REBUILD_FIRST''); end');
  targets = {@compiler.build.standaloneApplication};
  if ispc(); targets{end + 1} = @compiler.build.standaloneWindowsApplication; end
  oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  for k = 1:numel(targets)
    build = targets{k};
    directory = fullfile(work, int2str(k));
    options = compiler.build.StandaloneApplicationOptions(entry, ...
      'OutputDir', directory, 'ExecutableName', 'rebuild_app');
    filewrite(entry, 'function rebuild_entry(); disp(''REBUILD_FIRST''); end');
    first = build(options);
    hashes = output_hashes(directory);
    filewrite(fullfile(directory, 'user.txt'), 'USER CONTENT');
    mkdir(fullfile(directory, 'user-folder'));
    again = build(options);
    asserts.isequal(output_hashes(directory), hashes);
    asserts.isequal(first.Files, again.Files);
    filewrite(entry, 'function rebuild_entry(); disp(''REBUILD_SECOND''); end');
    second = build(options);
    asserts.isfalse(strcmp(sha256(second.Files{1}, '-file'), hashes{1}));
    [status, output] = system(['"', second.Files{1}, '"'], 60);
    asserts.istrue(status == 0 && contains(output, 'REBUILD_SECOND'), output);
    check_rejection(build, options, directory, entry);
    if ispc()
      alternate = @compiler.build.standaloneWindowsApplication;
      if k == 2; alternate = @compiler.build.standaloneApplication; end
      switched = alternate(options);
      asserts.isfalse(strcmp(switched.BuildType, first.BuildType));
      contract = jsondecode(__nelsonc_executable__('launcher-contract', switched.Files{1}));
      expectedKind = 'windowed';
      if k == 2; expectedKind = 'console'; end
      asserts.isequal(contract.kind, expectedKind);
    end
    options.EmbedArchive = false;
    external = build(options);
    archive = fullfile(directory, 'rebuild_app.nca');
    asserts.istrue(isfile(archive));
    asserts.isequal(numel(external.Files), 3);
    options.EmbedArchive = true;
    embedded = build(options);
    asserts.isfalse(isfile(archive));
    asserts.isequal(embedded.Files, first.Files);
    asserts.isequal(fileread(fullfile(directory, 'user.txt')), 'USER CONTENT');
    asserts.istrue(isfolder(fullfile(directory, 'user-folder')));
    asserts.isfalse(isfolder(fullfile(directory, '.nelsonc-publish')));
    if ispc()
      installer = fullfile(work, ['installer-', int2str(k)]);
      compiler.package.installer(embedded, 'RuntimeDelivery', 'none', 'OutputDir', installer);
      asserts.istrue(isfile(fullfile(installer, 'MyAppInstaller.exe')));
      stale = fullfile(work, ['stale-installer-', int2str(k)]);
      rejected(@() compiler.package.installer(first, 'RuntimeDelivery', 'none', 'OutputDir', stale));
      asserts.isfalse(isfolder(stale));
    end
    filewrite(archive, 'UNRELATED ARCHIVE');
    options.EmbedArchive = false;
    rejected(@() build(options));
    asserts.isequal(fileread(archive), 'UNRELATED ARCHIVE');
    [status, output] = system(['"', embedded.Files{1}, '"'], 60);
    asserts.istrue(status == 0 && contains(output, 'REBUILD_SECOND'), output);
  end
  clear restoreRuntime;
  clear cleanup;
end

function check_rejection(build, options, directory, entry)
  before = output_hashes(directory);
  filewrite(entry, 'function rebuild_entry(); unresolved_rebuild_dependency(); end');
  rejected(@() build(options));
  asserts.isequal(output_hashes(directory), before);
  filewrite(entry, 'function rebuild_entry(); disp(''REBUILD_SECOND''); end');
  readme = fullfile(directory, 'readme.txt');
  text = fileread(readme);
  filewrite(readme, 'USER EDIT');
  rejected(@() build(options));
  asserts.isequal(fileread(readme), 'USER EDIT');
  filewrite(readme, text);
  reportFile = fullfile(directory, 'buildresult.json');
  text = fileread(reportFile);
  report = jsondecode(text);
  report.applicationName = 'different_app';
  filewrite(reportFile, jsonencode(report));
  rejected(@() build(options));
  filewrite(reportFile, '{broken');
  rejected(@() build(options));
  rmfile(reportFile);
  rejected(@() build(options));
  filewrite(reportFile, text);
  asserts.isequal(output_hashes(directory), before);
end

function hashes = output_hashes(directory)
  names = {'rebuild_app', 'readme.txt', 'buildresult.json'};
  if ispc(); names{1} = [names{1}, '.exe']; end
  hashes = cell(1, numel(names));
  for k = 1:numel(names); hashes{k} = sha256(fullfile(directory, names{k}), '-file'); end
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end

function remove_work(work)
  allowed = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(work), [allowed, '/']));
  if isfolder(work); rmdir(work, 's'); end
end
