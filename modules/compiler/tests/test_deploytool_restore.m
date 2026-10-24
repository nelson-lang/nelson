%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--TIMEOUT 180-->
ncc('--help');
try
check_restore();

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_restore()
  work = tempname(fullfile(nelsonroot(), 'temp'));
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  entry = fullfile(work, 'editor_restore.m');
  filewrite(entry, 'function editor_restore(); disp(''EDITOR_RESTORED_OK''); end');
  window = __nelsonc_editor__('off');
  closeWindow = onCleanup(@() close_editor(window));
  state = window.UserData;
  state.Controls.Build.AppFile.Value = entry;
  state.Controls.Build.OutputDir.Value = fullfile(work, 'build');
  state.Controls.Installer.OutputDir.Value = fullfile(work, 'installer');
  asserts.istrue(__nelsonc_editor_action__(window, 'changed'));
  asserts.istrue(__nelsonc_editor_action__(window, 'build'), window.UserData.Status.Text);
  firstHash = sha256(window.UserData.Result.Files{1}, '-file');
  filewrite(entry, 'function editor_restore(); disp(''EDITOR_REBUILT_OK''); end');
  asserts.istrue(__nelsonc_editor_action__(window, 'build'), window.UserData.Status.Text);
  asserts.isfalse(strcmp(sha256(window.UserData.Result.Files{1}, '-file'), firstHash));
  state = window.UserData;
  asserts.istrue(isa(state.Result, 'compiler.build.Results'));
  asserts.istrue(~isempty(state.Project.LastBuild.ReportFile));
  executable = state.Result.Files{1};
  filename = fullfile(work, 'editor.ncproj');
  asserts.istrue(__nelsonc_editor_action__(window, 'save', filename));
  asserts.istrue(__nelsonc_editor_action__(window, 'new'));
  assert_local(entry);
  rmfile(entry);
  asserts.istrue(__nelsonc_editor_action__(window, 'open', filename));
  state = window.UserData;
  asserts.istrue(isstruct(state.Result), state.Status.Text);
  asserts.isequal(state.Result.Files{1}, executable);
  asserts.istrue(contains(state.Status.Text, 'Restored build:'));
  asserts.isfalse(state.Busy);
  asserts.isequal(state.Table.Data(:, 1), state.Result.Files);
  if ispc()
    asserts.isequal(char(state.Buttons{7}.Enable), 'on');
    asserts.istrue(__nelsonc_editor_action__(window, 'package'), window.UserData.Status.Text);
    asserts.istrue(isfile(fullfile(work, 'installer', 'MyAppInstaller.exe')));
  end
  asserts.istrue(__nelsonc_editor_action__(window, 'new'));
  filewrite(executable, 'DAMAGED APPLICATION');
  asserts.istrue(__nelsonc_editor_action__(window, 'open', filename));
  state = window.UserData;
  asserts.istrue(isempty(state.Result));
  asserts.isequal(char(state.Buttons{7}.Enable), 'off');
  asserts.isfalse(state.Busy);
  asserts.istrue(contains(state.Status.Text, 'unavailable'));
  asserts.isequal(state.Project.Build.AppFile, strrep(entry, '\', '/'));
  asserts.istrue(contains(strjoin(state.Log.Value, char(10)), 'changed'));
  state.Controls.Build.ExecutableName.Value = 'next_build';
  asserts.istrue(__nelsonc_editor_action__(window, 'changed'));
  asserts.isequal(window.UserData.Project.LastBuild, __nelsonc_project_result__('empty'));
  asserts.istrue(__nelsonc_editor_action__(window, 'save'));
  loaded = __nelsonc_project__('load', filename);
  asserts.istrue(isempty(__nelsonc_project__('restore-build', loaded)));
  asserts.istrue(__nelsonc_editor_action__(window, 'close'));
  clear closeWindow;
  clear cleanup;
end

function close_editor(window)
  if isgraphics(window); delete(window); end
end

function assert_local(filename)
  allowed = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(filename), [allowed, '/']));
end

function remove_work(work)
  if isfolder(work); assert_local(work); rmdir(work, 's'); end
end
