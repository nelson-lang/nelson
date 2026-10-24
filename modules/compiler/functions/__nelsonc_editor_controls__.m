%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [controls, target, resources, forms] = __nelsonc_editor_controls__(tabs, window, project)
  tab = uitab(tabs, 'Title', 'Application');
  forms = {uipanel(tab, 'BorderType', 'none')};
  buildNames = {'AppFile', 'ExecutableName', 'OutputDir', 'ExecutableVersion', ...
    'ExecutableIcon', 'CustomHelpTextFile', 'RuntimeLogFile', 'AutoDetectDataFiles', 'EmbedArchive', 'TreatInputsAsNumeric', 'Verbose'};
  grid = field_grid(forms{1}, numel(buildNames) + 1);
  grid.RowHeight = repmat({26}, 1, numel(buildNames) + 1);
  grid.RowSpacing = 4;
  label = uilabel(grid, 'Text', 'Application type');
  label.Layout.Row = 1;
  label.Layout.Column = 1;
  targets = {'standaloneApplication'};
  titles = {'Standalone application'};
  if ispc(); targets{end + 1} = 'standaloneWindowsApplication'; titles{end + 1} = 'Windows application (no console)'; end
  target = uidropdown(grid, 'Items', titles, 'ItemsData', targets, ...
    'ValueChangedFcn', @(source, event) __nelsonc_editor_action__(window, 'changed'));
  target.Layout.Row = 1;
  target.Layout.Column = [2, 3];
  controls.Build = fields(grid, window, 'Build', project.Build, buildNames, 1);
  tab = uitab(tabs, 'Title', 'Resources');
  resources = {file_list(tab, window, 'Build', 'Application resources'), ...
    file_list(tab, window, 'Installer', 'Additional installer files')};
  controls.Build.AdditionalFiles = resources{1}.List;
  controls.Installer.AdditionalFiles = resources{2}.List;
  tab = uitab(tabs, 'Title', 'Packages');
  forms{end + 1} = uipanel(tab, 'BorderType', 'none');
  grid = field_grid(forms{end}, 2);
  grid.RowHeight = {28, '1x'};
  label = uilabel(grid, 'Text', 'Support packages');
  label.Layout.Row = 1; label.Layout.Column = 1;
  callback = @(source, event) __nelsonc_editor_action__(window, 'changed');
  mode = uidropdown(grid, 'Items', {'autodetect', 'none', 'selected'}, 'ValueChangedFcn', callback);
  mode.Layout.Row = 1; mode.Layout.Column = [2, 3];
  choices = uilistbox(grid, 'Items', {}, 'Multiselect', 'on', 'ValueChangedFcn', callback);
  choices.Layout.Row = 2; choices.Layout.Column = [1, 3];
  controls.Build.SupportPackageMode = mode;
  controls.Build.SupportPackages = choices;
  groups = {{'ApplicationName', 'InstallerName', 'OutputDir', 'RuntimeDelivery', 'PackageType', ...
    'OptionalDependencies', 'Compression', 'DefaultInstallationDir', 'Shortcut', 'Verbose'}, ...
    {'Version', 'AuthorName', 'AuthorEmail', 'AuthorCompany', 'Summary', 'Description', 'InstallationNotes'}, ...
    {'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}};
  titles = {'Installer', 'Installer details', 'Installer images'};
  for j = 1:numel(groups)
    tab = uitab(tabs, 'Title', titles{j});
    forms{end + 1} = uipanel(tab, 'BorderType', 'none');
    names = groups{j};
    grid = field_grid(forms{end}, numel(names));
    values = fields(grid, window, 'Installer', project.Installer, names, 0);
    for k = 1:numel(names); controls.Installer.(names{k}) = values.(names{k}); end
  end
end

function grid = field_grid(tab, rows)
  grid = uigridlayout(tab, [rows, 3]);
  grid.RowHeight = repmat({28}, 1, rows);
  grid.ColumnWidth = {190, '1x', 36};
  grid.RowSpacing = 6;
end

function controls = fields(grid, window, section, values, names, offset)
  controls = struct();
  for k = 1:numel(names)
    key = names{k};
    text = regexprep(key, '([a-z])([A-Z])', '$1 $2');
    if strcmp(key, 'AppFile'); text = 'Main file'; end
    label = uilabel(grid, 'Text', text);
    label.Layout.Row = k + offset;
    label.Layout.Column = 1;
    choices = {};
    switch key
      case 'RuntimeDelivery'; choices = {'none', 'installer'};
      case 'PackageType'; choices = {'auto', 'zip'};
      case 'OptionalDependencies'; choices = {'all', 'none'};
      case 'Compression'; choices = {'none', 'fast', 'normal', 'max'};
    end
    callback = @(source, event) __nelsonc_editor_action__(window, 'changed');
    if islogical(values.(key))
      control = uicheckbox(grid, 'Text', '', 'Value', values.(key), 'ValueChangedFcn', callback);
    elseif ~isempty(choices)
      control = uidropdown(grid, 'Items', choices, 'Value', values.(key), 'ValueChangedFcn', callback);
    elseif any(strcmp(key, {'Description', 'InstallationNotes'}))
      control = uitextarea(grid, 'Value', {''}, 'ValueChangedFcn', callback);
      heights = grid.RowHeight;
      heights{k + offset} = 68;
      grid.RowHeight = heights;
    else
      control = uieditfield(grid, 'Value', values.(key), 'ValueChangedFcn', callback);
    end
    control.Tag = ['compiler_', section, '_', key];
    control.Layout.Row = k + offset;
    control.Layout.Column = 2;
    controls.(key) = control;
    if any(strcmp(key, {'AppFile', 'OutputDir', 'ExecutableIcon', 'CustomHelpTextFile', ...
      'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon', 'Shortcut'}))
      button = uibutton(grid, 'Text', '', 'Tooltip', ['Browse: ', text], ...
        'Icon', __nelsonc_editor_icon__('action-folder.svg'), ...
        'ButtonPushedFcn', @(source, event) __nelsonc_editor_action__(window, 'browse', section, key));
      button.Layout.Row = k + offset;
      button.Layout.Column = 3;
    else
      control.Layout.Column = [2, 3];
    end
  end
end

function area = file_list(parent, window, section, title)
  area.Panel = uipanel(parent, 'BorderType', 'none');
  area.Label = uilabel(area.Panel, 'Text', title, 'FontWeight', 'bold');
  area.List = uilistbox(area.Panel, 'Items', {}, 'Multiselect', 'on', 'Tag', ['compiler_', section, '_files']);
  area.Buttons = {};
  area.Buttons{1} = uibutton(area.Panel, 'Text', 'Add file', 'ButtonPushedFcn', ...
    @(source, event) __nelsonc_editor_action__(window, 'add-file', section));
  area.Buttons{2} = uibutton(area.Panel, 'Text', 'Add folder', 'ButtonPushedFcn', ...
    @(source, event) __nelsonc_editor_action__(window, 'add-folder', section));
  area.Buttons{3} = uibutton(area.Panel, 'Text', 'Remove', 'ButtonPushedFcn', ...
    @(source, event) __nelsonc_editor_action__(window, 'remove-file', section));
end
