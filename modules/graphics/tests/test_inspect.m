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
%=============================================================================
f = figure('Visible', 'off');
p = plot(1:10, (1:10) .^ 2);
drawnow();
fh = __web_graphics_handle__(f, 'number');
ph = __web_graphics_handle__(p, 'number');
%=============================================================================
% Full property model of a plotted object.
response = jsondecode(__web_figure_action__(fh, ...
  jsonencode(struct('action', 'inspect', 'objectHandle', ph)), false));
model = response.model;
asserts.isequal(model.type, 'line');
asserts.istrue(numel(model.properties) > 30);
names = cell(1, numel(model.properties));
kinds = cell(1, numel(model.properties));
for k = 1:numel(model.properties)
  entry = model.properties(k);
  names{k} = entry.name;
  kinds{k} = entry.kind;
end
asserts.istrue(any(strcmp(names, 'LineWidth')));
asserts.istrue(any(strcmp(names, 'Color')));
asserts.istrue(any(strcmp(names, 'Visible')));
%=============================================================================
% Property kinds, choices and writable flags.
lineWidth = model.properties(find(strcmp(names, 'LineWidth'), 1));
asserts.isequal(lineWidth.kind, 'number');
asserts.istrue(logical(lineWidth.writable));
color = model.properties(find(strcmp(names, 'Color'), 1));
asserts.isequal(color.kind, 'color');
visible = model.properties(find(strcmp(names, 'Visible'), 1));
asserts.isequal(visible.kind, 'enum');
asserts.isequal(visible.value, 'on');
asserts.istrue(any(strcmp(visible.choices, 'on')));
asserts.istrue(any(strcmp(visible.choices, 'off')));
%=============================================================================
% Read-only properties are reported but not editable.
typeEntry = model.properties(find(strcmp(names, 'Type'), 1));
asserts.isfalse(logical(typeEntry.writable));
%=============================================================================
% Object hierarchy tree.
asserts.isequal(response.tree.type, 'figure');
asserts.istrue(numel(response.tree.children) >= 1);
%=============================================================================
% Editing any writable property through the generic path.
__web_figure_action__(fh, jsonencode(struct('action', 'properties', ...
  'objectHandle', ph, 'properties', struct('LineWidth', 4))), false);
asserts.isequal(get(p, 'LineWidth'), 4);
__web_figure_action__(fh, jsonencode(struct('action', 'properties', ...
  'objectHandle', ph, 'properties', struct('Marker', 'o'))), false);
asserts.isequal(get(p, 'Marker'), 'o');
%=============================================================================
% Editing a read-only property is rejected.
failed = false;
try
  __web_figure_action__(fh, jsonencode(struct('action', 'properties', ...
    'objectHandle', ph, 'properties', struct('Type', 'bogus'))), false);
catch err
  failed = true;
  asserts.isequal(err.identifier, 'Nelson:graphics:readOnlyProperty');
end
asserts.istrue(failed);
%=============================================================================
% inspect(obj) records a one-shot target surfaced through the figure state.
inspect(p);
state = __web_figure_state_full__(fh);
asserts.isequal(state.inspector.target, ph);
asserts.isfalse(logical(state.inspector.close));
% The target is consumed once (-1 means no pending request; figures are 0-based).
state2 = __web_figure_state_full__(fh);
asserts.isequal(state2.inspector.target, -1);
%=============================================================================
% inspect -close emits a one-shot close request.
inspect('-close');
state3 = __web_figure_state_full__(fh);
asserts.istrue(logical(state3.inspector.close));
state4 = __web_figure_state_full__(fh);
asserts.isfalse(logical(state4.inspector.close));
%=============================================================================
% inspect on an object without a figure errors.
failedNoFig = false;
try
  inspect(groot);
catch
  failedNoFig = true;
end
asserts.istrue(failedNoFig);
%=============================================================================
% inspect targeting the figure itself returns the figure model.
figureModel = jsondecode(__web_figure_action__(fh, ...
  jsonencode(struct('action', 'inspect', 'objectHandle', 0)), false));
asserts.isequal(figureModel.model.type, 'figure');
%=============================================================================
close(f);
%=============================================================================
