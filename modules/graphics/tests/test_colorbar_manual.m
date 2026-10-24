%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
[f, ax] = make_axes();
cb = colorbar(ax, 'manual');
asserts.isequal(cb.Location, 'manual');
asserts.isequal(cb.AxisLocation, 'out');
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.815], 1e-12);
asserts.isequal(cb.Position, [0 0 1 1]);
basePosition = cb.Position;
figPosition = f.Position;
cb.Units = 'pixels';
asserts.isapprox(cb.Position, [1 1 figPosition(3) figPosition(4)], 1e-12);
asserts.istrue(any(abs(cb.Position - basePosition) > 1e-12));
cb.Units = 'normalized';
asserts.isapprox(cb.Position, basePosition, 1e-12);
cb.TickDirection = 'out';
asserts.isequal(cb.TickDirection, 'out');
cb.TickDirection = 'both';
asserts.isequal(cb.TickDirection, 'both');
cb.TickDirection = 'none';
asserts.isequal(cb.TickDirection, 'none');
cb.TickDirection = 'in';
asserts.isequal(cb.TickDirection, 'in');
cb.Color = [0.2 0.3 0.4];
asserts.isequal(cb.Color, [0.2 0.3 0.4]);
cb.FontName = 'Arial';
asserts.isequal(cb.FontName, 'Arial');
cb.FontAngle = 'italic';
asserts.isequal(cb.FontAngle, 'italic');
cb.FontWeight = 'bold';
asserts.isequal(cb.FontWeight, 'bold');
cb.LineWidth = 2;
asserts.isequal(cb.LineWidth, 2);
cb.TickLength = 0.02;
asserts.isequal(cb.TickLength, 0.02);
cb.Visible = 'off';
asserts.isequal(char(cb.Visible), 'off');
cb.Visible = 'on';
cb.HandleVisibility = 'off';
asserts.isequal(char(cb.HandleVisibility), 'off');
cb.HandleVisibility = 'on';
cb.UserData = magic(2);
asserts.isequal(cb.UserData, magic(2));
cb.Selected = 'on';
asserts.isequal(char(cb.Selected), 'on');
cb.SelectionHighlight = 'off';
asserts.isequal(char(cb.SelectionHighlight), 'off');
cb.HitTest = 'off';
asserts.isequal(char(cb.HitTest), 'off');
cb.PickableParts = 'none';
asserts.isequal(cb.PickableParts, 'none');
cb.BusyAction = 'cancel';
asserts.isequal(cb.BusyAction, 'cancel');
cb.Interruptible = 'off';
asserts.isequal(char(cb.Interruptible), 'off');
cb.ContextMenu = gobjects(0, 0);
asserts.isequal(cb.ContextMenu, gobjects(0, 0));
cb = colorbar(ax, 'Location', 'layout');
asserts.isequal(cb.Location, 'layout');
asserts.istrue(isgraphics(cb.Layout, 'layoutoptions'));
cb.Layout.Tile = 'west';
asserts.isequal(cb.Layout.Tile, 'west');
close(f);
%=============================================================================
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
