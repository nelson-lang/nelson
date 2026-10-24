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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
%=============================================================================
locations3d = {'eastoutside'; 'westoutside'; 'northoutside'; 'southoutside'; ...
  'east'; 'west'; 'north'; 'south'};
expectedAxes3d = {[0.11708963911525 0.11 0.698015133876601 0.815]; ...
  [0.216426076833527 0.11 0.698015133876601 0.815]; ...
  [0.13 0.0918375241779497 0.775 0.680473887814313]; ...
  [0.13 0.256905222437137 0.775 0.680473887814313]; ...
  [0.13 0.11 0.775 0.815]; [0.13 0.11 0.775 0.815]; ...
  [0.13 0.11 0.775 0.815]; [0.13 0.11 0.775 0.815]};
expectedColorBars3d = {[0.858562281722934 0.11 0.0248350795498642 0.815]; ...
  [0.148125727590221 0.11 0.0248350795498642 0.815]; ...
  [0.13 0.844535783365571 0.775 0.0412637008381689]; ...
  [0.13 0.143423597678917 0.775 0.0412637008381689]; ...
  [0.867747380675204 0.130631850419084 0.0248350795498642 0.773736299161831]; ...
  [0.142417539774932 0.130631850419084 0.0248350795498642 0.773736299161831]; ...
  [0.142417539774932 0.863104448742746 0.750164920450136 0.0412637008381689]; ...
  [0.142417539774932 0.130631850419084 0.750164920450136 0.0412637008381689]};
for k = 1:length(locations3d)
  [f, ax] = make_surf_axes();
  cb = colorbar(ax, locations3d{k});
  asserts.isequal(cb.Location, locations3d{k});
  asserts.isapprox(ax.Position, expectedAxes3d{k}, 1e-12);
  asserts.isapprox(cb.Position, expectedColorBars3d{k}, 1e-12);
  close(f);
end
%=============================================================================
locationsSequence3d = {'north'; 'south'; 'east'; 'west'; ...
  'northoutside'; 'southoutside'; 'eastoutside'; 'westoutside'};
[f, ax] = make_surf_axes_with_position([100 100 800 600]);
for k = 1:length(locationsSequence3d)
  colorbar(ax, locationsSequence3d{k});
  drawnow();
end
asserts.isapprox(ax.Position, [0.208933333333333 0.220933333333333 ...
  0.609666666666667 0.583177777777778], 1e-4);
asserts.isequal(ax.XLim, [0 50]);
asserts.isequal(ax.XTick, 0:10:50);
asserts.isequal(ax.YLim, [0 60]);
asserts.isequal(ax.YTick, [0 20 40 60]);
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isequal(ax.ZTick, [-10 -5 0 5 10]);
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
function [f, ax] = make_surf_axes_with_position(position)
  f = figure('Visible', 'off', 'Position', position);
  ax = axes('Parent', f);
  surf(ax, peaks);
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
