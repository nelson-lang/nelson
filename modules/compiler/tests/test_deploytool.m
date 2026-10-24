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
check_editor();

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_editor()
  directory = tempname();
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  window = __nelsonc_editor__('off');
  closeWindow = onCleanup(@() close_editor(window));
  asserts.isequal(window.Tag, 'nelsonCompilerEditor');
  standaloneApplicationCompiler();
  deploytool();
  asserts.isequal(__nelsonc_editor__('off'), window);
  asserts.isequal(numel(findall(0, 'Tag', 'nelsonCompilerEditor')), 1);
  state = window.UserData;
  asserts.isequal(char(state.Buttons{7}.Enable), 'off');
  entry = fullfile(directory, 'editor_entry.m');
  filewrite(entry, 'function editor_entry(); disp(''EDITOR_ENTRY_OK''); end');
  buildOptions = compiler.build.StandaloneApplicationOptions(entry);
  installerOptions = compiler.package.InstallerOptions('ApplicationName', 'EditorTest');
  asserts.isequal(sort(setdiff(fieldnames(state.Controls.Build), {'SupportPackageMode'})), sort(properties(buildOptions)));
  asserts.isequal(sort(fieldnames(state.Controls.Installer)), sort(properties(installerOptions)));
  state.Controls.Build.AppFile.Value = entry;
  state.Controls.Build.OutputDir.Value = fullfile(directory, 'build');
  state.Controls.Build.TreatInputsAsNumeric.Value = true;
  state.Controls.Build.SupportPackageMode.Value = 'none';
  asserts.istrue(__nelsonc_editor_action__(window, 'changed'));
  asserts.istrue(contains(window.Name, '*'));
  projectFile = fullfile(directory, 'editor.ncproj');
  asserts.istrue(__nelsonc_editor_action__(window, 'save', projectFile));
  asserts.isfalse(contains(window.Name, '*'));
  state = window.UserData;
  feval(state.Buttons{5}.ButtonPushedFcn, state.Buttons{5}, []);
  state = window.UserData;
  asserts.istrue(size(state.Table.Data, 1) > 0);
  asserts.isfalse(isfolder(fullfile(directory, 'build')));
  feval(state.Buttons{6}.ButtonPushedFcn, state.Buttons{6}, []);
  state = window.UserData;
  asserts.istrue(isa(state.Result, 'compiler.build.Results'), state.Status.Text);
  asserts.istrue(isfile(state.Result.Files{1}));
  asserts.istrue(state.Result.Options.TreatInputsAsNumeric);
  asserts.isequal(state.Result.Options.SupportPackages, {'none'});
  asserts.isequal(char(state.Buttons{7}.Enable), 'on');
  asserts.istrue(contains(strjoin(state.Command.Value, char(10)), 'compiler.build.standaloneApplication'));
  state.Controls.Installer.AuthorName.Value = 'Test author';
  __nelsonc_editor_action__(window, 'changed');
  state = window.UserData;
  asserts.istrue(isa(state.Result, 'compiler.build.Results'));
  state.Controls.Installer.OutputDir.Value = fullfile(directory, 'installer');
  feval(state.Buttons{7}.ButtonPushedFcn, state.Buttons{7}, []);
  state = window.UserData;
  extension = '.install';
  if ispc(); extension = '.exe'; elseif ismac(); extension = '.pkg'; end
  asserts.istrue(isfile(fullfile(directory, 'installer', ['MyAppInstaller', extension])), state.Status.Text);
  exported = fullfile(directory, 'build_script.m');
  asserts.istrue(__nelsonc_editor_action__(window, 'export', exported));
  asserts.istrue(contains(fileread(exported), 'compiler.build.standaloneApplication'));
  asserts.istrue(contains(fileread(exported), 'TreatInputsAsNumeric'));
  asserts.istrue(contains(fileread(exported), 'options.SupportPackages = {''none''};'));
  before = sha256(entry, '-file');
  asserts.isfalse(__nelsonc_editor_action__(window, 'export', entry));
  asserts.isequal(sha256(entry, '-file'), before);
  asserts.istrue(__nelsonc_editor_action__(window, 'build'));
  asserts.isequal(sha256(entry, '-file'), before);
  filewrite(entry, 'function editor_entry(); unresolved_editor_dependency(); end');
  asserts.isfalse(__nelsonc_editor_action__(window, 'build'));
  filewrite(entry, 'function editor_entry(); disp(''EDITOR_ENTRY_OK''); end');
  state = window.UserData;
  asserts.isfalse(state.Busy);
  asserts.isequal(char(state.Buttons{6}.Enable), 'on');
  asserts.istrue(isa(state.Result, 'compiler.build.Results'));
  state.Controls.Build.ExecutableName.Value = 'changed_entry';
  __nelsonc_editor_action__(window, 'changed');
  state = window.UserData;
  asserts.istrue(isempty(state.Result));
  asserts.isequal(char(state.Buttons{7}.Enable), 'off');
  __nelsonc_editor_action__(window, 'save');
  asserts.istrue(__nelsonc_editor_action__(window, 'new'));
  asserts.istrue(__nelsonc_editor_action__(window, 'open', projectFile));
  state = window.UserData;
  asserts.isequal(state.Controls.Build.ExecutableName.Value, 'changed_entry');
  previous = state.Project;
  broken = fullfile(directory, 'broken.ncproj');
  filewrite(broken, '{"invalid":');
  asserts.isfalse(__nelsonc_editor_action__(window, 'open', broken));
  asserts.isequal(window.UserData.Project, previous);
  window.Position = [100, 80, 800, 680];
  feval(window.SizeChangedFcn, window, []);
  assert_layout(window);
  window.Position = [100, 80, 1080, 760];
  feval(window.SizeChangedFcn, window, []);
  assert_layout(window);
  drawnow();
  frame = getframe(window);
  asserts.isequal(size(frame.cdata), [760, 1080, 3]);
  asserts.istrue(numel(unique(frame.cdata(:))) > 20);
  asserts.istrue(__nelsonc_editor_action__(window, 'close'));
  asserts.isfalse(isgraphics(window));
  reopened = __nelsonc_editor__('off');
  asserts.istrue(isgraphics(reopened));
  delete(reopened);
  clear closeWindow;
  clear cleanup;
end

function close_editor(window)
  if isgraphics(window); delete(window); end
end

function assert_layout(window)
  state = window.UserData;
  bounds = window.Position;
  for k = 1:7
    position = state.Buttons{k}.Position;
    asserts.istrue(position(1) >= 0 && position(2) >= 0);
    asserts.istrue(position(1) + position(3) <= bounds(3));
    asserts.istrue(position(2) + position(4) <= bounds(4));
    asserts.istrue(~isempty(state.Buttons{k}.Icon));
  end
  asserts.isequal(state.Tabs.Position(3), bounds(3) - 24);
  asserts.isequal(state.Forms{1}.Position(3), bounds(3) - 28);
  for name = {'TreatInputsAsNumeric', 'Verbose'}
    control = state.Controls.Build.(name{1});
    position = control.Position;
    parentBounds = control.Parent.Position;
    asserts.istrue(position(2) >= 0);
    asserts.istrue(position(2) + position(4) <= parentBounds(4));
  end
  asserts.istrue(state.Log.Position(2) + state.Log.Position(4) < state.Tabs.Position(2));
end
