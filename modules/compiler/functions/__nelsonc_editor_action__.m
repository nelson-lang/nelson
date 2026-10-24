%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function success = __nelsonc_editor_action__(window, action, varargin)
  success = false;
  if ~isgraphics(window); return; end
  state = window.UserData;
  if state.Busy; return; end
  try
    if strcmp(action, 'refresh')
      apply_project(state);
    else
      state = collect_project(state);
      switch action
        case 'changed'
          state.Status.Text = 'Modified';
        case {'close', 'new', 'open'}
          [proceed, state] = discard_changes(window, state);
          window.UserData = state;
          update_title(window, state);
          if ~proceed; return; end
          if strcmp(action, 'close'); delete(window); success = true; return; end
          if strcmp(action, 'new')
            state.Project = __nelsonc_project__('new');
            state.ProjectFile = '';
          else
            filename = chosen_file(varargin, '*.ncproj', 'Open compiler project');
            if isempty(filename); return; end
            state.Project = __nelsonc_project__('load', filename);
            state.ProjectFile = filename;
          end
          state.Saved = jsonencode(state.Project);
          state.Result = [];
          state.Sources = __nelsonc_project_sources__();
          state.Table.Data = cell(0, 2);
          apply_project(state);
          state.Status.Text = 'Ready';
          if strcmp(action, 'open') && ~isempty(state.Project.LastBuild.ReportFile)
            state.Busy = true;
            window.UserData = state;
            state.Status.Text = 'Restoring build...';
            set_buttons(state, 'off');
            drawnow();
            state = restore_build(state);
            state.Busy = false;
          end
        case {'save', 'saveas'}
          state = save_project(state, action, varargin);
        case 'set-project'
          state.Project = __nelsonc_project__('validate', varargin{1});
          state.Result = [];
          state.Sources = __nelsonc_project_sources__();
          apply_project(state);
        case {'browse', 'add-file', 'add-folder', 'remove-file'}
          state = __nelsonc_editor_files__(state, action, varargin{:});
          state = collect_project(state);
        case {'analyze', 'build', 'package'}
          state.Busy = true;
          window.UserData = state;
          state.Status.Text = [action, '...'];
          set_buttons(state, 'off');
          drawnow();
          state = perform_operation(state, action);
          state.Busy = false;
        case 'export'
          command = __nelsonc_project__('command', state.Project);
          if isempty(varargin)
            [name, folder] = uiputfile('*.m', 'Export build script', 'build_application.m');
            if isequal(name, 0); return; end
            filename = fullfile(folder, name);
          else
            filename = varargin{1};
          end
          if isfile(filename) || isfolder(filename)
            error('Nelson:compiler:project:exportDestinationExists', 'Export destination already exists; choose a new build script file.');
          end
          filewrite(filename, command);
          state.Status.Text = ['Exported: ', filename];
        otherwise
          error('Nelson:compiler:editor:unknownAction', 'Unknown editor action.');
      end
    end
    success = true;
  catch ex
    state.Busy = false;
    if strcmp(ex.identifier, 'Nelson:compiler:staleBuild')
      state.Sources = struct('State', 'changed', 'Message', ex.message);
    end
    state.Status.Text = ex.message;
    state.Log.Value = [state.Log.Value(:); {[ex.identifier, ': ', ex.message]}];
  end
  if isgraphics(window)
    set_buttons(state, 'on');
    update_title(window, state);
    try
      state.Command.Value = strsplit(__nelsonc_project__('command', state.Project), char(10));
    catch
      state.Command.Value = {''};
    end
    window.UserData = state;
  end
end

function state = collect_project(state)
  project = state.Project;
  for section = {'Build', 'Installer'}
    group = section{1};
    names = fieldnames(state.Controls.(group));
    for k = 1:numel(names)
      key = names{k};
      control = state.Controls.(group).(key);
      if strcmp(key, 'SupportPackageMode'); continue; end
      if strcmp(key, 'AdditionalFiles')
        value = control.Items;
      elseif strcmp(key, 'SupportPackages')
        mode = state.Controls.Build.SupportPackageMode.Value;
        control.Enable = 'off';
        if strcmp(mode, 'selected')
          control.Enable = 'on';
          value = control.Value;
          if ischar(value); value = {value}; end
        else
          value = {mode};
        end
      else
        value = control.Value;
        if iscell(value); value = strjoin(value, char(10)); end
      end
      project.(group).(key) = value;
    end
  end
  project.Target = state.Target.Value;
  if ~strcmp(jsonencode(project.Build), jsonencode(state.Project.Build)) || ~strcmp(project.Target, state.Project.Target)
    state.Result = [];
    project.LastBuild = __nelsonc_project_result__('empty');
    state.Sources = __nelsonc_project_sources__();
  end
  state.Project = project;
end

function apply_project(state)
  for section = {'Build', 'Installer'}
    group = section{1};
    names = fieldnames(state.Controls.(group));
    for k = 1:numel(names)
      key = names{k};
      control = state.Controls.(group).(key);
      if strcmp(key, 'SupportPackageMode'); continue; end
      value = state.Project.(group).(key);
      if strcmp(key, 'AdditionalFiles')
        control.Items = value;
      elseif strcmp(key, 'SupportPackages')
        catalog = __nelsonc_package_registry__();
        selected = ~isequal(value, {'autodetect'}) && ~isequal(value, {'none'});
        control.Enable = 'off';
        if selected
          control.Items = unique([{catalog.Name}, value]);
          control.Value = value;
          control.Enable = 'on';
          state.Controls.Build.SupportPackageMode.Value = 'selected';
        else
          control.Items = unique({catalog.Name});
          control.Value = {};
          state.Controls.Build.SupportPackageMode.Value = value{1};
        end
      elseif any(strcmp(key, {'Description', 'InstallationNotes'}))
        control.Value = strsplit(value, char(10));
      else
        control.Value = value;
      end
    end
  end
  state.Target.Value = state.Project.Target;
end

function state = perform_operation(state, action)
  state.Project = __nelsonc_project__('validate', state.Project);
  apply_project(state);
  switch action
    case 'analyze'
      plan = __nelsonc_project__('analyze', state.Project);
      rows = cell(numel(plan.files), 2);
      for k = 1:numel(plan.files); rows(k, :) = {plan.files(k).source, plan.files(k).relative}; end
      state.Table.Data = rows;
      state.Status.Text = sprintf('%d files; %d runtime modules; %d unresolved dependencies', ...
        numel(plan.files), numel(plan.runtime.modules), numel(plan.diagnostics));
      for k = 1:numel(plan.diagnostics)
        state.Log.Value = [state.Log.Value(:); {jsonencode(plan.diagnostics(k))}];
      end
      state.Sources = __nelsonc_project_sources__(state.Project, plan);
      if ~strcmp(state.Sources.State, 'unbuilt')
        state.Log.Value = [state.Log.Value(:); {state.Sources.Message}];
      end
    case 'build'
      result = __nelsonc_project__('build', state.Project);
      state.Project = __nelsonc_project__('record-build', state.Project, result);
      state.Result = result;
      state.Sources = __nelsonc_project__('source-status', state.Project);
      state.Table.Data = [result.Files(:), repmat({''}, numel(result.Files), 1)];
      state.Status.Text = ['Built: ', result.Files{1}];
      if ~strcmp(state.Sources.State, 'current'); state.Status.Text = state.Sources.Message; end
    case 'package'
      if isempty(state.Result)
        error('Nelson:compiler:editor:buildBeforeInstaller', 'Build the current project before creating its installer.');
      end
      [directory, state.Sources] = __nelsonc_project__('package', state.Project, state.Result);
      state.Log.Value = [state.Log.Value(:); {state.Sources.Message}];
      state.Status.Text = ['Installer created: ', directory];
  end
  state.Log.Value = [state.Log.Value(:); {state.Status.Text}];
  state.Tabs.SelectedTab = state.ResultTab;
end

function state = restore_build(state)
  try
    state.Result = __nelsonc_project__('restore-build', state.Project);
    if ~isempty(state.Result)
      state.Table.Data = [state.Result.Files(:), repmat({''}, numel(state.Result.Files), 1)];
      state.Sources = state.Result.Sources;
      captions = struct('current', 'sources unchanged', 'changed', 'sources changed', ...
        'unavailable', 'sources unavailable', 'unknown', 'source status unknown');
      state.Status.Text = ['Restored build: ', captions.(state.Sources.State)];
      state.Log.Value = [state.Log.Value(:); {state.Sources.Message}];
    end
  catch ex
    state.Result = [];
    state.Status.Text = 'Project opened; saved build unavailable.';
    state.Log.Value = [state.Log.Value(:); {[ex.identifier, ': ', ex.message]}];
  end
end

function state = save_project(state, action, args)
  filename = state.ProjectFile;
  if ~isempty(args)
    filename = args{1};
  elseif isempty(filename) || strcmp(action, 'saveas')
    [name, folder] = uiputfile('*.ncproj', 'Save compiler project', 'application.ncproj');
    if isequal(name, 0); return; end
    filename = fullfile(folder, name);
  end
  % Native Save As confirms replacement; explicit paths only replace this project.
  replace = isempty(args) || strcmp(filename, state.ProjectFile);
  state.ProjectFile = __nelsonc_project__('save', state.Project, filename, replace);
  state.Project = __nelsonc_project__('validate', state.Project);
  apply_project(state);
  state.Saved = jsonencode(state.Project);
  state.Status.Text = ['Saved: ', state.ProjectFile];
end

function [yes, state] = discard_changes(window, state)
  yes = strcmp(state.Saved, jsonencode(state.Project));
  if yes; return; end
  answer = uiconfirm(window, 'Save changes to this project?', 'Unsaved project', ...
    'Options', {'Save', 'Discard', 'Cancel'}, 'DefaultOption', 'Save', 'CancelOption', 'Cancel');
  if strcmp(answer, 'Discard'); yes = true; return; end
  if strcmp(answer, 'Save')
    state = save_project(state, 'save', {});
    yes = ~isempty(state.ProjectFile) && strcmp(state.Saved, jsonencode(state.Project));
  end
end

function filename = chosen_file(args, filter, title)
  filename = '';
  if ~isempty(args); filename = args{1}; return; end
  [name, folder] = uigetfile(filter, title);
  if ~isequal(name, 0); filename = fullfile(folder, name); end
end

function update_title(window, state)
  title = 'Untitled';
  if ~isempty(state.ProjectFile)
    [folder, name, extension] = fileparts(state.ProjectFile);
    title = [name, extension];
  end
  if ~strcmp(state.Saved, jsonencode(state.Project)); title = [title, ' *']; end
  state.ProjectLabel.Text = title;
  window.Name = ['Nelson Standalone Application Compiler - ', title];
end

function set_buttons(state, enabled)
  for k = 1:numel(state.Buttons); state.Buttons{k}.Enable = enabled; end
  state.Target.Enable = enabled;
  for group = {'Build', 'Installer'}
    controls = state.Controls.(group{1});
    names = fieldnames(controls);
    for k = 1:numel(names); controls.(names{k}).Enable = enabled; end
  end
  if isempty(state.Result) || strcmp(state.Sources.State, 'changed'); state.Buttons{7}.Enable = 'off'; end
end
