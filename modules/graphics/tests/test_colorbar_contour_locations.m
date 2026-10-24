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
contourLocations = {'southoutside'; 'northoutside'; 'south'; 'north'};
expectedContourAxes = {[0.13 0.245996131528046 0.775 0.690473887814313]; ...
  [0.13 0.0959187620889749 0.775 0.71068665377176]; ...
  [0.13 0.11 0.775 0.815]; [0.13 0.11 0.775 0.815]};
expectedContourColorBars = {[0.13 0.144177949709865 0.775 0.0412637008381689]; ...
  [0.13 0.84215667311412 0.775 0.0412637008381689]; ...
  [0.142417539774932 0.130631850419084 0.750164920450136 0.0412637008381689]; ...
  [0.142417539774932 0.863104448742746 0.750164920450136 0.0412637008381689]};
for k = 1:length(contourLocations)
  [f, ax] = make_contour_axes();
  cb = colorbar(ax, contourLocations{k});
  asserts.isequal(cb.Location, contourLocations{k});
  asserts.isapprox(ax.Position, expectedContourAxes{k}, 1e-12);
  asserts.isapprox(cb.Position, expectedContourColorBars{k}, 1e-12);
  close(f);
end
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
