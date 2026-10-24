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
check_editor_sources();

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_editor_sources()
  work = tempname(fullfile(nelsonroot(), 'temp'));
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  entry = fullfile(work, 'editor_sources.m');
  original = 'function editor_sources(); disp(''FIRST''); end';
  replacement = 'function editor_sources(); disp(''OTHER''); end';
  filewrite(entry, original);
  window = __nelsonc_editor__('off');
  closeWindow = onCleanup(@() close_editor(window));
  state = window.UserData;
  state.Controls.Build.AppFile.Value = entry;
  state.Controls.Build.OutputDir.Value = fullfile(work, 'build');
  state.Controls.Installer.OutputDir.Value = fullfile(work, 'installer');
  asserts.istrue(__nelsonc_editor_action__(window, 'build'), window.UserData.Status.Text);
  asserts.isequal(window.UserData.Sources.State, 'current');
  savedDigest = window.UserData.Project.LastBuild.SourcesSHA256;
  filename = fullfile(work, 'project.ncproj');
  asserts.istrue(__nelsonc_editor_action__(window, 'save', filename));
  filewrite(entry, replacement);
  asserts.isfalse(__nelsonc_editor_action__(window, 'package'));
  asserts.isequal(window.UserData.Sources.State, 'changed');
  asserts.isequal(char(window.UserData.Buttons{7}.Enable), 'off');
  asserts.isfalse(window.UserData.Busy);
  asserts.isfalse(isfolder(fullfile(work, 'installer')));
  asserts.istrue(__nelsonc_editor_action__(window, 'new'));
  asserts.istrue(__nelsonc_editor_action__(window, 'open', filename));
  asserts.isequal(window.UserData.Sources.State, 'changed');
  asserts.istrue(contains(window.UserData.Status.Text, 'changed'));
  asserts.isequal(char(window.UserData.Buttons{7}.Enable), 'off');
  filewrite(entry, original);
  asserts.istrue(__nelsonc_editor_action__(window, 'analyze'));
  asserts.isequal(window.UserData.Sources.State, 'current');
  if ispc(); asserts.isequal(char(window.UserData.Buttons{7}.Enable), 'on'); end
  filewrite(entry, replacement);
  asserts.istrue(__nelsonc_editor_action__(window, 'build'), window.UserData.Status.Text);
  asserts.isequal(window.UserData.Sources.State, 'current');
  asserts.isfalse(strcmp(window.UserData.Project.LastBuild.SourcesSHA256, savedDigest));
  asserts.istrue(__nelsonc_editor_action__(window, 'save'));
  root = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(entry), [root, '/']));
  rmfile(entry);
  asserts.istrue(__nelsonc_editor_action__(window, 'new'));
  asserts.istrue(__nelsonc_editor_action__(window, 'open', filename));
  asserts.isequal(window.UserData.Sources.State, 'unavailable');
  asserts.istrue(contains(strjoin(window.UserData.Log.Value, char(10)), 'verification unavailable'));
  if ispc()
    asserts.isequal(char(window.UserData.Buttons{7}.Enable), 'on');
    asserts.istrue(__nelsonc_editor_action__(window, 'package'), window.UserData.Status.Text);
    asserts.istrue(isfile(fullfile(work, 'installer', 'MyAppInstaller.exe')));
  end
  asserts.istrue(__nelsonc_editor_action__(window, 'close'));
  clear closeWindow;
  clear cleanup;
end

function close_editor(window)
  if isgraphics(window); delete(window); end
end

function remove_work(work)
  root = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(work), [root, '/']));
  if isfolder(work); rmdir(work, 's'); end
end
