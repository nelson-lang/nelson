%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function window = __nelsonc_editor__(visible)
  persistent current;
  if nargin == 0; visible = 'on'; end
  if ~isempty(current) && isgraphics(current)
    window = current;
    window.Visible = visible;
    if strcmp(visible, 'on'); figure(window); drawnow(); end
    return;
  end
  if isempty(which('uifigure'))
    error('Nelson:compiler:graphics', 'The compiler editor requires a graphical Nelson session.');
  end
  window = uifigure('Name', 'Nelson Standalone Application Compiler', ...
    'Position', [100, 80, 1080, 760], 'Visible', 'off', 'Tag', 'nelsonCompilerEditor');
  try
    state = create_editor(window);
    window.UserData = state;
    window.AutoResizeChildren = 'off';
    window.SizeChangedFcn = @(source, event) __nelsonc_editor_layout__(window);
    __nelsonc_editor_layout__(window);
    window.CloseRequestFcn = @(source, event) __nelsonc_editor_action__(window, 'close');
    __nelsonc_editor_action__(window, 'refresh');
    window.Visible = visible;
    current = window;
    if strcmp(visible, 'on'); figure(window); drawnow(); end
  catch ex
    delete(window);
    rethrow(ex);
  end
end

function state = create_editor(window)
  tools = window;
  state.Buttons = {};
  actions = {'new', 'open', 'save', 'saveas'};
  labels = {'New project', 'Open project', 'Save project', 'Save project as'};
  icons = {'document-new.svg', 'action-folder.svg', 'action-save.svg', 'document-save-as.svg'};
  for k = 1:numel(actions)
    state.Buttons{end + 1} = tool_button(tools, window, actions{k}, '', labels{k}, icons{k});
  end
  state.ProjectLabel = uilabel(tools, 'Text', 'Untitled', 'FontWeight', 'bold');
  state.Buttons{end + 1} = tool_button(tools, window, 'analyze', 'Analyze', 'Analyze dependencies', 'action-search.svg');
  state.Buttons{end + 1} = tool_button(tools, window, 'build', 'Build', 'Build application', 'action-run.svg');
  state.Buttons{end}.BackgroundColor = [0.12, 0.40, 0.27];
  state.Buttons{end}.FontColor = [1, 1, 1];
  state.Buttons{end + 1} = tool_button(tools, window, 'package', 'Installer', 'Create installer', 'action-package.svg');
  state.Tabs = uitabgroup(window);
  state.Project = __nelsonc_project__('new');
  [state.Controls, state.Target, state.Resources, state.Forms] = __nelsonc_editor_controls__(state.Tabs, window, state.Project);
  resultTab = uitab(state.Tabs, 'Title', 'Results');
  resultPanel = uipanel(resultTab, 'BorderType', 'none');
  state.Forms{end + 1} = resultPanel;
  resultGrid = uigridlayout(resultPanel, [3, 1]);
  resultGrid.RowHeight = {'1x', 30, 150};
  state.Table = uitable(resultGrid, 'Data', cell(0, 2), 'ColumnName', {'Source / output', 'Archive path'}, ...
    'ColumnWidth', {520, 'auto'}, 'RowName', {});
  state.Buttons{end + 1} = tool_button(resultGrid, window, 'export', 'Export build script', 'Export build script', 'action-save.svg');
  state.Command = uitextarea(resultGrid, 'Editable', 'off', 'Value', {''}, 'FontName', 'Consolas');
  state.Log = uitextarea(window, 'Editable', 'off', 'Value', {''});
  state.Status = uilabel(window, 'Text', 'Ready');
  state.ResultTab = resultTab;
  state.ProjectFile = '';
  state.Saved = jsonencode(state.Project);
  state.Result = [];
  state.Sources = __nelsonc_project_sources__();
  state.Busy = false;
end

function button = tool_button(parent, window, action, label, tooltip, icon)
  button = uibutton(parent, 'Text', label, 'Tooltip', tooltip, 'Tag', ['compiler_', action], ...
    'Icon', __nelsonc_editor_icon__(icon), ...
    'ButtonPushedFcn', @(source, event) __nelsonc_editor_action__(window, action));
end
