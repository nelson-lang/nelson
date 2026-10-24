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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
%=============================================================================
[f, ax] = make_axes();
cb = colorbar(ax);
asserts.istrue(isgraphics(cb, 'colorbar'));
asserts.isequal(cb.Type, 'colorbar');
asserts.isequal(cb.Tag, 'Colorbar');
asserts.isequal(cb.Parent, f);
asserts.isequal(length(cb.Children), 0);
asserts.istrue(isgraphics(cb.Label, 'text'));
asserts.isequal(cb.Label.Parent, cb);
asserts.isequal(cb.Label.String, '');
asserts.isequal(cb.Location, 'eastoutside');
asserts.isequal(cb.AxisLocation, 'out');
asserts.isequal(cb.AxisLocationMode, 'auto');
asserts.isequal(cb.LimitsMode, 'auto');
asserts.isequal(cb.TicksMode, 'auto');
asserts.isequal(cb.TickLabelsMode, 'auto');
asserts.isequal(cb.TickLabelInterpreter, 'tex');
asserts.isequal(cb.Direction, 'normal');
asserts.isequal(cb.FontSize, 9);
asserts.isequal(char(cb.Box), 'on');
asserts.isequal(char(cb.Visible), 'on');
asserts.isequal(char(cb.HandleVisibility), 'on');
asserts.isapprox(ax.Position, [0.121117578579744 0.11 0.722066356228172 0.815], 1e-12);
asserts.isapprox(cb.Position, [0.855721769499418 0.11 0.0248350795498642 0.815], 1e-12);
asserts.istrue(isgraphics(cb.Layout, 'layoutoptions'));
assert_required_properties(cb, reference_colorbar_properties());
s = get(cb);
asserts.istrue(isstruct(s));
asserts.istrue(isfield(s, 'Label'));
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
