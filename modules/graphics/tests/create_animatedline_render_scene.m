%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [f, markerLine, tail, clipLine, sceneFrame, x] = create_animatedline_render_scene()
  f = figure('Visible', 'off', 'Units', 'pixels', 'InnerPosition', [100 100 420 320], ...
    'Color', [1 1 1]);
  ax = axes('Parent', f, 'Position', [0.16 0.16 0.74 0.72]);
  set(ax, 'XLim', [0 6.5], 'YLim', [-1.4 1.4], 'Box', 'on', 'Color', [1 1 1]);
  hold(ax, 'on');
  x = 0:0.5:6;
  markerLine = animatedline(ax, x, sin(x), 'Color', [0.86 0.12 0.12], ...
    'LineWidth', 2.5, 'LineStyle', '-.', 'Marker', 'o', 'MarkerSize', 7, ...
    'MarkerFaceColor', [0.1 0.7 0.2], 'MarkerEdgeColor', [0 0 0]);
  tail = animatedline(ax, 'MaximumNumPoints', 4, 'Color', [0 0.35 0.8], ...
    'LineWidth', 4, 'LineStyle', '-', 'Marker', 'none');
  addpoints(tail, x, cos(x));
  clipLine = animatedline(ax, [-1 1 7], [0.8 -0.8 0.8], 'Color', [0 0 0], ...
    'LineWidth', 2, 'Clipping', 'off');
  drawnow();
  sceneFrame = getframe(f).cdata;
end
