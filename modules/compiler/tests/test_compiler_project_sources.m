%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 240-->
ncc('--help');
check_sources();

function check_sources()
  work = [tempname(fullfile(nelsonroot(), 'temp')), ' sources '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  root = fullfile(work, 'project');
  source = fullfile(root, 'source');
  assets = fullfile(root, 'assets');
  folders = fullfile(root, 'folders');
  mkdir(source); mkdir(assets); mkdir(folders);
  entry = fullfile(source, 'freshness_entry.m');
  helper = fullfile(source, 'freshness_helper.m');
  data = fullfile(assets, 'data.txt');
  helpFile = fullfile(root, 'help.txt');
  filewrite(entry, 'function freshness_entry(); disp(freshness_helper()); disp(hilb(2)); end');
  filewrite(helper, 'function text = freshness_helper(); text = ''FIRST''; end');
  filewrite(data, 'FIRST');
  filewrite(helpFile, 'FIRST HELP');
  project = __nelsonc_project__('new');
  asserts.isequal(__nelsonc_project__('source-status', project).State, 'unbuilt');
  project.Build.AppFile = entry;
  project.Build.AdditionalFiles = {fullfile(assets, '*.txt'), folders};
  project.Build.CustomHelpTextFile = helpFile;
  project.Build.OutputDir = fullfile(root, 'build');
  project.Installer.OutputDir = fullfile(root, 'installer');
  if ispc()
    icon = fullfile(root, 'icon.png');
    copyfile(fullfile(modulepath('compiler'), 'examples', 'standalone', 'app_icon.png'), icon);
    project.Build.ExecutableIcon = icon;
  end
  result = __nelsonc_project__('build', project);
  project = __nelsonc_project__('record-build', project, result);
  asserts.isequal(numel(project.LastBuild.SourcesSHA256), 64);
  check_status(project, 'current');
  if ispc()
    tool = fullfile(modulepath('compiler'), 'tests', 'test_compiler_source_timestamps.ps1');
    [status, text] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', tool, '" -Directory "', source, '"'], 30);
    asserts.isequal(status, 0, text);
    check_status(project, 'current');
    snapshot = result.__nelsonc_report__();
    asserts.isequal(snapshot.DependencyPlan.executableIconSHA256, sha256(icon, '-file'));
  end
  check_file(project, result, entry, 'function freshness_entry(); disp(''SECOND''); end');
  check_file(project, result, helper, 'function text = freshness_helper(); text = ''OTHER''; end');
  check_file(project, result, data, 'OTHER');
  check_file(project, result, helpFile, 'OTHER HELP');
  check_graph(project, assets, folders);
  if ispc()
    backup = fullfile(work, 'original.png');
    copyfile(icon, backup);
    filewrite(icon, 'CHANGED ICON BYTES');
    check_status(project, 'changed');
    copyfile(backup, icon);
    check_status(project, 'current');
  end
  % Recording uses the built snapshot even when input bytes have changed since build.
  original = fileread(helper);
  filewrite(helper, 'function text = freshness_helper(); text = ''LATER''; end');
  later = __nelsonc_project__('record-build', project, result);
  asserts.isequal(later.LastBuild.SourcesSHA256, project.LastBuild.SourcesSHA256);
  check_status(later, 'changed');
  filewrite(helper, original);
  check_status(project, 'current');
  filename = fullfile(root, 'application.ncproj');
  __nelsonc_project__('save', project, filename);
  legacy = jsondecode(fileread(filename));
  legacy.Version = 3;
  legacy.Build = rmfield(legacy.Build, {'TreatInputsAsNumeric', 'SupportPackages'});
  legacy.LastBuild.SettingsSHA256 = __nelsonc_project_result__('fingerprint', legacy);
  legacy.LastBuild = rmfield(legacy.LastBuild, 'SourcesSHA256');
  old = fullfile(root, 'legacy.ncproj');
  filewrite(old, jsonencode(legacy));
  migrated = __nelsonc_project__('load', old);
  asserts.isequal(migrated.Version, 6);
  asserts.isequal(__nelsonc_project__('restore-build', migrated).Sources.State, 'unknown');
  malformed = project;
  malformed.LastBuild.SourcesSHA256 = 'invalid';
  rejected(@() __nelsonc_project__('validate', malformed));
  moved = fullfile(work, 'moved');
  assert_local(root); assert_local(work);
  [ok, message] = movefile(root, moved);
  asserts.istrue(ok, message);
  loaded = __nelsonc_project__('load', fullfile(moved, 'application.ncproj'));
  asserts.isequal(loaded.LastBuild.SourcesSHA256, project.LastBuild.SourcesSHA256);
  check_status(loaded, 'current');
  original = fileread(loaded.Build.AppFile);
  filewrite(loaded.Build.AppFile, 'function broken(');
  check_status(loaded, 'unavailable');
  filewrite(loaded.Build.AppFile, original);
  source = fullfile(moved, 'source');
  remove_work(source);
  check_status(loaded, 'unavailable');
  restored = __nelsonc_project__('restore-build', loaded);
  asserts.isequal(restored.Sources.State, 'unavailable');
  if ispc()
    [directory, status] = __nelsonc_project__('package', loaded, restored);
    asserts.isequal(status.State, 'unavailable');
    asserts.istrue(isfile(fullfile(directory, 'MyAppInstaller.exe')));
  end
  clear cleanup;
end

function check_file(project, result, filename, replacement)
  original = fileread(filename);
  filewrite(filename, replacement);
  check_status(project, 'changed');
  restored = __nelsonc_project__('restore-build', project);
  asserts.isequal(restored.Sources.State, 'changed');
  rejected(@() __nelsonc_project__('package', project, result));
  rejected(@() __nelsonc_project__('package', project, restored));
  asserts.isfalse(isfolder(project.Installer.OutputDir));
  filewrite(filename, original);
  check_status(project, 'current');
end

function check_graph(project, assets, folders)
  unselected = fullfile(assets, 'ignored.log');
  filewrite(unselected, 'UNSELECTED');
  check_status(project, 'current');
  added = fullfile(assets, 'added.txt');
  filewrite(added, 'NEW RESOURCE');
  check_status(project, 'changed');
  assert_local(added); rmfile(added);
  check_status(project, 'current');
  added = fullfile(folders, 'new-empty-folder');
  mkdir(added);
  check_status(project, 'changed');
  remove_work(added);
  check_status(project, 'current');
end

function check_status(project, expected)
  beforeDirectory = pwd();
  beforePath = path();
  actual = __nelsonc_project__('source-status', project);
  asserts.isequal(actual.State, expected, actual.Message);
  asserts.isequal(pwd(), beforeDirectory);
  asserts.isequal(path(), beforePath);
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end

function assert_local(filename)
  root = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(filename), [root, '/']));
end

function remove_work(work)
  assert_local(work);
  if isfolder(work); rmdir(work, 's'); end
end
