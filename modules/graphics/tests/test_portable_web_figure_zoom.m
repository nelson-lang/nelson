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
% Centre zoom (focus at the middle keeps the view centre fixed).
f = figure('Visible', 'off', 'Position', [20 30 400 300]);
ax = axes('Parent', f, 'XLim', [0 10], 'YLim', [0 20]);
plot(ax, [0 10], [0 20]);
drawnow();
__web_figure_action__(__web_graphics_handle__(f, 'number'), jsonencode(struct('action', 'zoom', ...
  'factor', 2, 'focusXFrac', 0.5, 'focusYFrac', 0.5)));
assert_isapprox(ax.XLim, [2.5 7.5], 1e-12);
assert_isapprox(ax.YLim, [5 15], 1e-12);
close(f);
%=============================================================================
% Cursor-anchored zoom keeps the point under the cursor fixed.
f = figure('Visible', 'off', 'Position', [20 30 400 300]);
ax = axes('Parent', f, 'XLim', [0 10], 'YLim', [0 20]);
plot(ax, [0 10], [0 20]);
drawnow();
% Bottom-left corner: X fraction 0, screen Y fraction 1 (= data Y fraction 0).
__web_figure_action__(__web_graphics_handle__(f, 'number'), jsonencode(struct('action', 'zoom', ...
  'factor', 2, 'focusXFrac', 0, 'focusYFrac', 1)));
assert_isapprox(ax.XLim, [0 5], 1e-12);
assert_isapprox(ax.YLim, [0 10], 1e-12);
close(f);
%=============================================================================
% Rubber-band region zoom (fractions of the axes rectangle).
f = figure('Visible', 'off', 'Position', [20 30 400 300]);
ax = axes('Parent', f, 'XLim', [0 10], 'YLim', [0 20]);
plot(ax, [0 10], [0 20]);
drawnow();
request = struct('action', 'zoom');
request.regionXFrac = [0.2 0.6];
request.regionYFrac = [0.25 0.75];
__web_figure_action__(__web_graphics_handle__(f, 'number'), jsonencode(request));
assert_isapprox(ax.XLim, [2 6], 1e-12);
assert_isapprox(ax.YLim, [5 15], 1e-12);
close(f);
%=============================================================================
% Reversed axis: the region still maps to the pointed data range.
f = figure('Visible', 'off', 'Position', [20 30 400 300]);
ax = axes('Parent', f, 'XLim', [0 10], 'YLim', [0 20], 'XDir', 'reverse');
plot(ax, [0 10], [0 20]);
drawnow();
request = struct('action', 'zoom');
request.regionXFrac = [0.2 0.6];
request.regionYFrac = [0 1];
__web_figure_action__(__web_graphics_handle__(f, 'number'), jsonencode(request));
assert_isapprox(ax.XLim, [4 8], 1e-12);
assert_isapprox(ax.YLim, [0 20], 1e-12);
close(f);
%=============================================================================
