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
t = tiledlayout(f, 2, 2);
ax1 = nexttile(t);
surf(peaks);
cb1 = colorbar(ax1);
title(ax1, 'Surface');
ax2 = nexttile(t);
contourf(peaks);
cb2 = colorbar(ax2, 'southoutside');
title(ax2, 'Horizontal');
ax3 = nexttile(t);
imagesc(peaks);
cb3 = colorbar(ax3, 'Ticks', [-6 -3 0 3 6], ...
  'TickLabels', {'low'; '-3'; '0'; '3'; 'high'});
cb3.Label.String = 'Scale';
cb3.Direction = 'reverse';
title(ax3, 'Ticks');
ax4 = nexttile(t);
pcolor(peaks);
cb4 = colorbar(ax4, 'Location', 'layout');
cb4.Layout.Tile = 'east';
title(ax4, 'Layout');
drawnow();
asserts.isapprox(cb1.Position, ...
  [0.488468501886028 0.616729231473708 0.0186219739292365 0.308270768526292], 1e-12);
asserts.isapprox(cb2.Position, ...
  [0.5875 0.530041925218206 0.3175 0.0309597523219815], 1e-12);
asserts.isapprox(cb3.Position, ...
  [0.459914649286158 0.11 0.0186219739292365 0.308270768526292], 1e-12);
asserts.isapprox(cb4.Position, ...
  [0.929829298572315 0.11 0.0186219739292365 0.815], 1e-12);
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
