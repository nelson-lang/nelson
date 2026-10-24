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
f = figure('Visible', 'off');
plot(0:7, [-1 1 -1 1 -1 1 -1 1]);
axis equal;
drawnow();
ax = gca();
asserts.isapprox(ax.XLim, [0 7], 1e-12);
asserts.istrue((ax.YLim(2) - ax.YLim(1)) <= 7);
close(f);
%=============================================================================
% square data (equal x and y extents) in a wide plot box: the plot box fills
% the available area (its aspect ratio matches the pixel box) and the shorter
% direction (here X) is expanded so that equal data units are preserved.
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
t = linspace(0, 2 * pi, 200);
plot(cos(t), sin(t));
axis equal;
drawnow();
ax = gca();
asserts.isapprox(ax.YLim, [-1 1], 1e-2);
asserts.istrue(ax.XLim(1) < -1.2 && ax.XLim(1) > -1.35);
asserts.istrue(ax.XLim(2) > 1.2 && ax.XLim(2) < 1.35);
% equal data units: the x extent equals the y extent scaled by the box aspect.
boxAspect = ax.PlotBoxAspectRatio(1) / ax.PlotBoxAspectRatio(2);
asserts.isapprox((ax.XLim(2) - ax.XLim(1)), (ax.YLim(2) - ax.YLim(1)) * boxAspect, 1e-6);
asserts.isequal(ax.DataAspectRatio, [1 1 1]);
close(f);
%=============================================================================
% 'axis([...]); axis equal' followed by adding data must keep the binding
% (data-limited) direction manual and free the other one to 'auto', so the
% framing stays encompassing and equal-unit instead of collapsing tight to the
% newly added object.
f = figure('Visible', 'off', 'Position', [80 80 700 500]);
ax = axes('Parent', f);
hold(ax, 'on');
axis(ax, [-3 3 -3 0.55]);
axis(ax, 'equal');
patch('Parent', ax, 'XData', [0 1 0.5], 'YData', [0 0 -2], 'FaceColor', [1 0 0]);
drawnow();
% X is the binding direction: kept manual at the requested framing.
asserts.isapprox(ax.XLim, [-3 3], 1e-9);
asserts.isequal(ax.XLimMode, 'manual');
% Y was freed to 'auto' and stretched to equal units around the data.
asserts.isequal(ax.YLimMode, 'auto');
asserts.isequal(ax.DataAspectRatio, [1 1 1]);
boxAspect = ax.PlotBoxAspectRatio(1) / ax.PlotBoxAspectRatio(2);
asserts.isapprox((ax.XLim(2) - ax.XLim(1)), (ax.YLim(2) - ax.YLim(1)) * boxAspect, 1e-6);
% The range still encompasses the added patch data (y in [-2, 0]).
asserts.istrue(ax.YLim(1) <= -2 && ax.YLim(2) >= 0);
close(f);
%=============================================================================
