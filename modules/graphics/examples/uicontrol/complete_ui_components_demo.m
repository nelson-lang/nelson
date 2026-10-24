%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Demonstrate modern UI components, containers and callbacks.
addpath([modulepath('graphics', 'root'), '/examples/uicontrol']);

% The components are laid out at fixed pixel positions, so the windows keep a
% fixed size: resizing would not reflow the absolute layout.
figureHandle = uifigure('Name', 'Complete UI component gallery', ...
  'Position', [120 80 1000 680], 'Resize', 'off', ...
  'Tag', 'completeUiComponentsFigure');
figureHandle.HandleVisibility = 'on';
componentFigure = uifigure('Name', 'UI containers and data components', ...
  'Position', [180 120 1000 680], 'Resize', 'off', ...
  'Tag', 'completeUiComponentGallery');
tabGroup = uitabgroup(componentFigure, 'Position', [10 10 980 660]);
componentsTab = uitab(tabGroup, 'Title', 'Component gallery');
layoutTab = uitab(tabGroup, 'Title', 'Grid layout');

axesHandle = uiaxes(figureHandle, 'Position', [320 125 630 470]);
x = linspace(0, 2 * pi, 300);
plotHandle = plot(axesHandle, x, sin(x), 'LineWidth', 2);
xlabel(axesHandle, 'x');
ylabel(axesHandle, 'signal');
grid(axesHandle, 'on');

demoMenu = uimenu(figureHandle, 'Text', 'Demo');
uimenu(demoMenu, 'Text', 'Reset signal', ...
  'MenuSelectedFcn', @completeUiComponentsReset);
contextMenu = uicontextmenu(figureHandle);
uimenu(contextMenu, 'Text', 'Reset signal', ...
  'MenuSelectedFcn', @completeUiComponentsReset);
axesHandle.ContextMenu = contextMenu;

uilabel(figureHandle, 'Text', 'Amplitude', 'Position', [25 555 110 22]);
amplitudeField = uieditfield(figureHandle, 'numeric', 'Value', 1, ...
  'Limits', [0.1 5], 'Position', [145 555 125 24], ...
  'Tag', 'completeUiModernAmplitude');
uilabel(figureHandle, 'Text', 'Frequency', 'Position', [25 500 110 22]);
frequencySlider = uislider(figureHandle, 'Limits', [0.5 6], 'Value', 1, ...
  'MajorTicks', [1 2 3 4 5 6], 'MinorTicks', 0.5:0.5:6, 'Position', [35 470 225 3], ...
  'Tag', 'completeUiModernFrequency');
uilabel(figureHandle, 'Text', 'Waveform', 'Position', [25 420 110 22]);
waveformDropdown = uidropdown(figureHandle, ...
  'Items', {'Sine', 'Cosine', 'Square', 'Triangle'}, ...
  'Value', 'Sine', 'Position', [145 420 125 24], ...
  'Tag', 'completeUiModernWaveform');
uilabel(figureHandle, 'Text', 'Phase (degrees)', 'Position', [25 372 110 22]);
phaseSpinner = uispinner(figureHandle, 'Limits', [0 360], 'Step', 15, ...
  'Value', 0, 'Position', [145 372 125 24], ...
  'Tag', 'completeUiModernPhase');
gridCheckbox = uicheckbox(figureHandle, 'Text', 'Show grid', 'Value', true, ...
  'Position', [25 325 110 24], 'Tag', 'completeUiModernGrid');

styleGroup = uibuttongroup(figureHandle, 'Title', 'Line style', ...
  'Position', [25 205 245 100]);
solidRadio = uiradiobutton(styleGroup, 'Text', 'Solid', 'Value', true, ...
  'Position', [18 50 90 24]);
dashedRadio = uiradiobutton(styleGroup, 'Text', 'Dashed', ...
  'Position', [125 50 90 24]);
visibilitySwitch = uiswitch(figureHandle, 'Items', {'Hidden', 'Visible'}, ...
  'Value', 'Visible', 'Position', [70 170 46 22], ...
  'Tag', 'completeUiModernVisibility');
statusLamp = uilamp(figureHandle, 'Color', [0.20 0.75 0.30], ...
  'Position', [84 148 18 18]);
frequencyKnob = uiknob(figureHandle, 'Limits', [0.5 6], 'Value', 1, ...
  'Position', [180 125 70 70], 'Tag', 'completeUiModernKnob');
frequencyGauge = uigauge(figureHandle, 'semicircular', 'Limits', [0.5 6], ...
  'Value', 1, 'Position', [25 45 130 85]);
resetButton = uibutton(figureHandle, 'Text', 'Reset', ...
  'Position', [175 48 90 30], 'Tag', 'completeUiModernReset');
statusLabel = uilabel(figureHandle, 'Text', 'Ready', ...
  'Position', [320 65 630 28], 'Tag', 'completeUiModernStatus');

indicatorPanel = uipanel(componentsTab, 'Title', 'Indicators and inputs', ...
  'Position', [20 325 300 270]);
uilabel(indicatorPanel, 'Text', 'Date', 'Position', [15 212 45 22]);
uidatepicker(indicatorPanel, 'Position', [70 212 210 24]);
uilabel(indicatorPanel, 'Text', 'Level', 'Position', [15 155 45 22]);
uiknob(indicatorPanel, 'discrete', 'Items', {'Low', 'Medium', 'High'}, ...
  'Value', 'Medium', 'Position', [82 102 62 62]);
uigauge(indicatorPanel, 'linear', 'Value', 65, ...
  'Position', [185 150 100 40]);
uilabel(indicatorPanel, 'Text', 'Power', 'Position', [15 42 45 22]);
uiswitch(indicatorPanel, 'rocker', 'Value', 'On', ...
  'Position', [96 22 24 46]);
uilamp(indicatorPanel, 'Color', [0.20 0.75 0.30], ...
  'Position', [200 40 22 22]);

dataPanel = uipanel(componentsTab, 'Title', 'Table and text', ...
  'Position', [340 325 630 270]);
uitable(dataPanel, 'Data', magic(4), 'ColumnName', {'A', 'B', 'C', 'D'}, ...
  'Position', [15 20 370 210]);
uitextarea(dataPanel, 'Value', {'Editable text area'; ...
  'Multiple lines are supported.'}, 'Position', [405 115 205 115]);
uilistbox(dataPanel, 'Items', {'Alpha', 'Beta', 'Gamma', 'Delta'}, ...
  'Value', 'Beta', 'Position', [405 20 205 80]);

treePanel = uipanel(componentsTab, 'Title', 'Tree and button group', ...
  'Position', [20 25 950 275]);
treeHandle = uitree(treePanel, 'Position', [15 20 250 215]);
signalsNode = uitreenode(treeHandle, 'Text', 'Signals');
uitreenode(signalsNode, 'Text', 'Sine');
uitreenode(signalsNode, 'Text', 'Cosine');
modelsNode = uitreenode(treeHandle, 'Text', 'Models');
uitreenode(modelsNode, 'Text', 'Reaction diffusion');
choiceGroup = uibuttongroup(treePanel, 'Title', 'Exclusive choices', ...
  'Position', [290 115 230 120]);
uiradiobutton(choiceGroup, 'Text', 'Option A', 'Position', [20 62 100 24]);
uiradiobutton(choiceGroup, 'Text', 'Option B', 'Position', [120 62 100 24]);
toggleGroup = uibuttongroup(treePanel, 'Title', 'Toggle buttons', ...
  'Position', [290 20 230 80]);
uitogglebutton(toggleGroup, 'Text', 'Left', 'Position', [20 25 85 28]);
uitogglebutton(toggleGroup, 'Text', 'Right', 'Position', [120 25 85 28]);
uibutton(treePanel, 'state', 'Text', 'State button', ...
  'Position', [570 175 130 35]);
uibutton(treePanel, 'Text', 'Push button', ...
  'Position', [570 120 130 35]);
uicheckbox(treePanel, 'Text', 'Check box', 'Value', true, ...
  'Position', [570 75 130 28]);
uidropdown(treePanel, 'Items', {'First', 'Second', 'Third'}, ...
  'Position', [740 175 160 28]);
uieditfield(treePanel, 'Value', 'Edit field', ...
  'Position', [740 125 160 28]);
uispinner(treePanel, 'Value', 5, 'Limits', [0 10], ...
  'Position', [740 75 160 28]);

layoutGrid = uigridlayout(layoutTab, [3 3]);
layoutGrid.RowHeight = {45, '1x', 55};
layoutGrid.ColumnWidth = {'1x', '2x', '1x'};
uilabel(layoutGrid, 'Text', 'Automatic component placement', ...
  'HorizontalAlignment', 'center');
uibutton(layoutGrid, 'Text', 'Button A');
uibutton(layoutGrid, 'Text', 'Button B');
uilistbox(layoutGrid, 'Items', {'One', 'Two', 'Three'});
uitable(layoutGrid, 'Data', magic(3));
uitextarea(layoutGrid, 'Value', {'Resizable cells'; 'Weighted rows and columns'});
uicheckbox(layoutGrid, 'Text', 'Enabled', 'Value', true);
uidropdown(layoutGrid, 'Items', {'Compact', 'Comfortable'});
uispinner(layoutGrid, 'Value', 3, 'Limits', [1 10]);

state = struct();
state.axes = axesHandle;
state.plot = plotHandle;
state.x = x;
state.amplitudeField = amplitudeField;
state.frequencySlider = frequencySlider;
state.waveformDropdown = waveformDropdown;
state.phaseSpinner = phaseSpinner;
state.gridCheckbox = gridCheckbox;
state.solidRadio = solidRadio;
state.dashedRadio = dashedRadio;
state.visibilitySwitch = visibilitySwitch;
state.statusLamp = statusLamp;
state.frequencyKnob = frequencyKnob;
state.frequencyGauge = frequencyGauge;
state.resetButton = resetButton;
state.statusLabel = statusLabel;
state.componentFigure = componentFigure;
figureHandle.UserData = state;

amplitudeField.ValueChangedFcn = @completeUiComponentsUpdate;
frequencySlider.ValueChangedFcn = @completeUiComponentsUpdate;
waveformDropdown.ValueChangedFcn = @completeUiComponentsUpdate;
phaseSpinner.ValueChangedFcn = @completeUiComponentsUpdate;
gridCheckbox.ValueChangedFcn = @completeUiComponentsUpdate;
styleGroup.SelectionChangedFcn = @completeUiComponentsUpdate;
visibilitySwitch.ValueChangedFcn = @completeUiComponentsUpdate;
frequencyKnob.ValueChangedFcn = @completeUiComponentsUpdate;
resetButton.ButtonPushedFcn = @completeUiComponentsReset;
completeUiComponentsUpdate(amplitudeField, []);
%=============================================================================
