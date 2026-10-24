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
check_results();

function check_results()
  work = [tempname(tempdir()), ' results '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  project = __nelsonc_project__('new');
  asserts.istrue(isempty(__nelsonc_project__('restore-build', project)));
  targets = {'standaloneApplication'};
  if ispc(); targets{end + 1} = 'standaloneWindowsApplication'; end
  for k = 1:numel(targets)
    root = fullfile(work, int2str(k));
    [project, result] = build_project(root, targets{k});
    unrelated = project;
    unrelated.Build.ExecutableName = 'unrelated';
    rejected(@() __nelsonc_project__('record-build', unrelated, result), 'Mismatched build options.');
    project = __nelsonc_project__('record-build', project, result);
    baseline = '';
    if ispc()
      reference = project;
      reference.Installer.OutputDir = fullfile(work, ['baseline-', int2str(k)]);
      baseline = __nelsonc_project__('package', reference, result);
    end
    filename = fullfile(root, 'application.ncproj');
    __nelsonc_project__('save', project, filename);
    saved = jsondecode(fileread(filename));
    asserts.isequal(saved.Version, 6);
    asserts.isequal(saved.LastBuild.ReportFile, './build/buildresult.json');
    restoredProject = __nelsonc_project__('load', filename);
    currentDirectory = pwd();
    currentPath = path();
    restored = __nelsonc_project__('restore-build', restoredProject);
    asserts.isequal(restored.Files{1}, result.Files{1});
    asserts.isequal(numel(restored.Files), 3);
    asserts.isequal(restored.Distribution.buildType, targets{k});
    asserts.isequal(restored.Sources.State, 'current');
    asserts.isequal(pwd(), currentDirectory);
    asserts.isequal(path(), currentPath);
    verify_failures(restoredProject, restored, root);
    elsewhere = fullfile(root, 'save-as');
    mkdir(elsewhere);
    alternative = fullfile(elsewhere, 'copy.ncproj');
    __nelsonc_project__('save', restoredProject, alternative);
    copy = __nelsonc_project__('load', alternative);
    asserts.isequal(__nelsonc_project__('restore-build', copy).Files, restored.Files);
    altered = restoredProject;
    altered.Build.ExecutableVersion = '2.0';
    rejected(@() __nelsonc_project__('restore-build', altered), 'Changed project settings.');
    __nelsonc_project__('save', altered, fullfile(root, 'changed.ncproj'));
    changed = __nelsonc_project__('load', fullfile(root, 'changed.ncproj'));
    asserts.isequal(changed.LastBuild, __nelsonc_project_result__('empty'));
    moved = fullfile(work, ['moved-', int2str(k)]);
    assert_local(root);
    assert_local(work);
    [ok, message] = movefile(root, moved);
    asserts.istrue(ok, message);
    asserts.isfalse(isfolder(root));
    project = __nelsonc_project__('load', fullfile(moved, 'application.ncproj'));
    restored = __nelsonc_project__('restore-build', project);
    asserts.istrue(startsWith(strrep(restored.Files{1}, '\', '/'), strrep(moved, '\', '/')));
    asserts.isequal(restored.Sources.State, 'current');
    source = fullfile(moved, 'source');
    assert_local(source);
    rmdir(source, 's');
    project = __nelsonc_project__('load', fullfile(moved, 'application.ncproj'));
    restored = __nelsonc_project__('restore-build', project);
    asserts.isequal(restored.Sources.State, 'unavailable');
    if ispc()
      output = __nelsonc_project__('package', project, restored);
      installer = fullfile(output, 'MyAppInstaller.exe');
      asserts.istrue(isfile(installer));
      asserts.isequal(sha256(installer, '-file'), sha256(fullfile(baseline, 'MyAppInstaller.exe'), '-file'));
      check_install(installer, fullfile(moved, 'installed'), project.Build.ExecutableName);
    end
    old = getenv('NELSONC_RUNTIME_ROOT');
    restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
    setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
    [status, text] = system(['"', restored.Files{1}, '"'], 60);
    asserts.isequal(status, 0, text);
    asserts.istrue(contains(text, 'RESTORED_PROJECT_OK:payload'), text);
    clear restoreRuntime;
  end
  clear cleanup;
end

function [project, result] = build_project(root, target)
  source = fullfile(root, 'source');
  mkdir(source);
  entry = fullfile(source, 'restore_entry.m');
  code = {'function restore_entry()', ...
    'if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end', ...
    'data = fileread(fullfile(fileparts(mfilename(''fullpath'')), ''data.txt''));', ...
    'disp([''RESTORED_PROJECT_OK:'', data]);', 'end'};
  filewrite(entry, strjoin(code, char(10)));
  filewrite(fullfile(source, 'data.txt'), 'payload');
  project = __nelsonc_project__('new');
  project.Target = target;
  project.Build.AppFile = entry;
  project.Build.ExecutableName = 'restored_app';
  project.Build.OutputDir = fullfile(root, 'build');
  project.Build.AdditionalFiles = {fullfile(source, 'data.txt')};
  project.Build.AutoDetectDataFiles = false;
  project.Build.EmbedArchive = false;
  project.Installer.OutputDir = fullfile(root, 'installer');
  result = __nelsonc_project__('build', project);
end

function verify_failures(project, restored, root)
  for k = 1:numel(restored.Files)
    filename = restored.Files{k};
    backup = [filename, '.backup'];
    copyfile(filename, backup);
    restoreFile = onCleanup(@() movefile(backup, filename, 'f'));
    filewrite(filename, 'CHANGED');
    rejected(@() __nelsonc_project__('restore-build', project), filename);
    if ispc()
      rejected(@() __nelsonc_project__('package', project, restored), 'Changed output before packaging.');
      asserts.isfalse(isfolder(project.Installer.OutputDir));
    end
    clear restoreFile;
    copyfile(filename, backup);
    restoreFile = onCleanup(@() movefile(backup, filename, 'f'));
    assert_local(filename);
    rmfile(filename);
    rejected(@() __nelsonc_project__('restore-build', project), 'Missing application output.');
    clear restoreFile;
  end
  report = project.LastBuild.ReportFile;
  backup = [report, '.backup'];
  copyfile(report, backup);
  restoreReport = onCleanup(@() movefile(backup, report, 'f'));
  filewrite(report, '{}');
  rejected(@() __nelsonc_project__('restore-build', project), 'Changed report.');
  clear restoreReport;
  missing = project;
  missing.LastBuild.ReportFile = fullfile(root, 'missing.json');
  rejected(@() __nelsonc_project__('restore-build', missing), 'Missing report.');
  for invalid = {42, struct(), struct('ReportFile', 'file.json'), ...
    struct('ReportFile', '', 'ReportSHA256', 'invalid', 'SettingsSHA256', '')}
    malformed = project;
    malformed.LastBuild = invalid{1};
    rejected(@() __nelsonc_project__('validate', malformed), 'Invalid saved build reference.');
  end
end

function check_install(installer, target, name)
  assert_local(fileparts(target));
  [status, text] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', target, '"'], 90);
  asserts.isequal(status, 0, text);
  uninstall = dir(fullfile(target, 'unins*.exe'));
  asserts.isequal(numel(uninstall), 1);
  cleanup = onCleanup(@() system(['"', fullfile(target, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90));
  old = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  [status, text] = system(['"', fullfile(target, [name, '.exe']), '"'], 60);
  asserts.isequal(status, 0, text);
  asserts.istrue(contains(text, 'RESTORED_PROJECT_OK:payload'), text);
  clear restoreRuntime;
  clear cleanup;
  asserts.isfalse(isfile(fullfile(target, [name, '.exe'])));
end

function rejected(callback, message)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed, message);
end

function assert_local(directory)
  allowed = tempdir();
  asserts.istrue(startsWith(fullpath(directory), allowed));
end

function remove_work(work)
  if isfolder(work); assert_local(work); rmdir(work, 's'); end
end
