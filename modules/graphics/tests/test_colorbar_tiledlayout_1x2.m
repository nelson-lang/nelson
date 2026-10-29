%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
t = tiledlayout(f, 1, 2);
ax1 = nexttile(t);
axes(ax1);
imagesc([0 1; 0.5 0.2]);
ax = nexttile(t);
axes(ax);
imagesc([0 1; 0.5 0.2]);
cb = colorbar(ax, 'Location', 'layout');
drawnow();
asserts.isequal(cb.Layout.Tile, 2);
asserts.isequal(cb.AxisLocation, 'out');
asserts.isapprox(ax.Position, [0.5875 0.11 0.3175 0.815], 1e-12);
asserts.isapprox(cb.Position, [0.731964285714286 0.11 0.0285714285714286 0.815], 1e-12);
cb.Layout.Tile = 'east';
drawnow();
asserts.isequal(cb.Layout.Tile, 'east');
asserts.isapprox(ax.Position, [0.5875 0.11 0.3175 0.815], 1e-12);
asserts.isapprox(cb.Position, [0.929829298572315 0.11 0.0186219739292365 0.815], 1e-12);
cb.Location = 'eastoutside';
drawnow();
asserts.isequal(cb.Location, 'eastoutside');
asserts.istrue(cb.Position(1) > ax.Position(1));
close(f);
%=============================================================================
function [f, ax] = make_axes()
  f = figure('Visible', 'off');
  ax = axes('Parent', f);
end
%=============================================================================
function [f, ax] = make_surf_axes()
  f = figure('Visible', 'off');
  ax = axes('Parent', f);
  [x, y, z] = peaks(50);
  surf(ax, x, y, z);
end
%=============================================================================
function [f, ax] = make_contour_axes()
  f = figure('Visible', 'off');
  contourf(peaks);
  ax = gca();
end
%=============================================================================
function tf = is_outside_location(location)
  tf = strcmp(location, 'northoutside') || strcmp(location, 'southoutside') || ...
    strcmp(location, 'eastoutside') || strcmp(location, 'westoutside');
end
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
end
%=============================================================================
function names = reference_colorbar_properties()
  names = {'Label'; 'Box'; 'Color'; 'Direction'; 'FontAngle'; 'FontName'; 'FontSize'; ...
    'FontWeight'; 'Limits'; 'LimitsMode'; 'LineWidth'; 'Location'; 'Position'; ...
    'AxisLocation'; 'AxisLocationMode'; 'TickDirection'; 'TickLabelInterpreter'; ...
    'TickLabels'; 'TickLabelsMode'; 'TickLength'; 'Ticks'; 'TicksMode'; 'Units'; ...
    'Children'; 'Parent'; 'Visible'; 'HandleVisibility'; 'ButtonDownFcn'; ...
    'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible'; 'CreateFcn'; ...
    'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected'; 'SelectionHighlight'; ...
    'HitTest'; 'PickableParts'; 'Layout'};
end
%=============================================================================
