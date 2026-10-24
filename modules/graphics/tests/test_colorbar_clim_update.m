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
clim(ax, [10 20]);
cb = colorbar(ax, 'Ticks', [10 15 20], 'TickLabels', {'low'; 'mid'; 'high'}, ...
  'TickLabelInterpreter', 'none', 'Direction', 'reverse', 'FontSize', 12);
asserts.isequal(cb.Limits, [10 20]);
asserts.isequal(cb.LimitsMode, 'auto');
asserts.isequal(cb.Ticks, [10 15 20]);
asserts.isequal(cb.TicksMode, 'manual');
asserts.isequal(cb.TickLabels, {'low'; 'mid'; 'high'});
asserts.isequal(cb.TickLabelsMode, 'manual');
asserts.isequal(cb.TickLabelInterpreter, 'none');
asserts.isequal(cb.Direction, 'reverse');
asserts.isequal(cb.FontSize, 12);
cb.Limits = [0 5];
asserts.isequal(cb.LimitsMode, 'manual');
cb.AxisLocation = 'in';
asserts.isequal(cb.AxisLocationMode, 'manual');
cb.Position = [0.2 0.2 0.05 0.6];
asserts.isequal(cb.Location, 'manual');
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
