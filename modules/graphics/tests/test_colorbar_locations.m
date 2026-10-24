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
locations = {'north'; 'south'; 'east'; 'west'; 'northoutside'; 'southoutside'; ...
  'eastoutside'; 'westoutside'};
for k = 1:length(locations)
  [f, ax] = make_axes();
  basePosition = ax.Position;
  cb = colorbar(ax, locations{k});
  asserts.isequal(cb.Location, locations{k});
  if is_outside_location(locations{k})
    asserts.isequal(cb.AxisLocation, 'out');
    asserts.istrue(any(abs(ax.Position - basePosition) > 1e-12));
    colorbar(cb, 'off');
    asserts.isapprox(ax.Position, basePosition, 1e-12);
  else
    asserts.isequal(cb.AxisLocation, 'in');
    asserts.isapprox(ax.Position, basePosition, 1e-12);
  end
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
