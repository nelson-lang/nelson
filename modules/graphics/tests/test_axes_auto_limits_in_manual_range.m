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
% With one manual X or Y range, the other auto limits fit the data visible in it.
f = figure('Visible', 'off');
ax = axes(f);
%=============================================================================
% Lines are clipped at the range edges.
plot(ax, 1:10);
set(ax, 'XLim', [2 3]);
drawnow();
assert_isequal(ax.YLimMode, 'auto');
assert_isapprox(ax.YLim, [2 3], 1e-12);
set(ax, 'XLim', [5 8]);
drawnow();
assert_isapprox(ax.YLim, [5 8], 1e-12);
set(ax, 'XLimMode', 'auto');
drawnow();
assert_isapprox(ax.YLim, [1 10], 1e-12);
%=============================================================================
cla(ax, 'reset');
line(ax, [0 100], [0 100]);
set(ax, 'XLim', [40 50]);
drawnow();
assert_isapprox(ax.YLim, [40 50], 1e-12);
%=============================================================================
% Marker-only lines are clipped like lines.
cla(ax, 'reset');
plot(ax, 1:10, (1:10) * 10, 'o');
set(ax, 'XLim', [2.5 4.5]);
drawnow();
assert_isapprox(ax.YLim, [25 45], 1e-12);
%=============================================================================
% Manual Y range drives the auto X limits.
cla(ax, 'reset');
plot(ax, 1:10, (1:10) * 10);
set(ax, 'YLim', [25 45]);
drawnow();
assert_isequal(ax.XLimMode, 'auto');
assert_isapprox(ax.XLim, [2.5 4.5], 1e-12);
%=============================================================================
% No data in range: default limits.
cla(ax, 'reset');
plot(ax, 1:10);
set(ax, 'XLim', [20 30]);
drawnow();
assert_isapprox(ax.YLim, [0 1], 1e-12);
%=============================================================================
% Lines outside the range do not count.
cla(ax, 'reset');
plot(ax, 1:10, 1:10);
hold(ax, 'on');
plot(ax, 100:110, 1000:1010);
set(ax, 'XLim', [1 10]);
drawnow();
assert_isapprox(ax.YLim, [1 10], 1e-12);
hold(ax, 'off');
%=============================================================================
% Patches are clipped as polygons.
cla(ax, 'reset');
patch(ax, [0 10 20], [0 100 0], 'r');
set(ax, 'XLim', [0 5]);
drawnow();
assert_isapprox(ax.YLim, [0 50], 1e-12);
%=============================================================================
% Zero-width patch inside the range and a long line crossing it.
cla(ax, 'reset');
plot(ax, 1:10);
set(ax, 'XLim', [1000 1001]);
patch(ax, [1000.5 1000.5 1000.5 1000.5], [2 2 5 5], 'r');
drawnow();
assert_isapprox(ax.YLim, [2 5], 1e-12);
patch(ax, [1 2 2 1], [1 1 2 2], 'r');
line(ax, [0 5000], [0 1]);
drawnow();
assert_isapprox(ax.YLim, [0 5], 1e-12);
%=============================================================================
% Scatter, stairs, area, errorbar and bar keep the points inside the range.
cla(ax, 'reset');
scatter(ax, 1:10, (1:10) * 3);
set(ax, 'XLim', [2 4]);
drawnow();
assert_isapprox(ax.YLim, [6 12], 1e-12);
cla(ax, 'reset');
stairs(ax, 1:10, (1:10) * 10);
set(ax, 'XLim', [2.5 4.5]);
drawnow();
assert_isapprox(ax.YLim, [30 40], 1e-12);
cla(ax, 'reset');
area(ax, 1:10, (1:10) * 10);
set(ax, 'XLim', [2.5 4.5]);
drawnow();
assert_isapprox(ax.YLim, [0 40], 1e-12);
cla(ax, 'reset');
errorbar(ax, 1:10, (1:10) * 10, ones(1, 10) * 5);
set(ax, 'XLim', [2.5 4.5]);
drawnow();
assert_isapprox(ax.YLim, [25 45], 1e-12);
cla(ax, 'reset');
bar(ax, 1:10, (1:10) * 10);
set(ax, 'YLimMode', 'auto', 'XLim', [2.5 4.5]);
drawnow();
assert_isapprox(ax.YLim, [0 40], 1e-12);
%=============================================================================
% Surfaces keep their full extent.
cla(ax, 'reset');
[X, Y] = meshgrid(0:10, 0:10);
surface(ax, X, X .* 10 + Y, zeros(size(X)));
set(ax, 'XLim', [2 3]);
drawnow();
assert_isapprox(ax.YLim, [0 120], 1e-12);
%=============================================================================
% Log X scale clips in log space: the data at x = 10 and x = 20 bound Y.
cla(ax, 'reset');
plot(ax, 1:100, 1:100);
set(ax, 'XScale', 'log', 'XLim', [10 20]);
drawnow();
assert_isapprox(ax.YLim, [10 20], 1e-12);
%=============================================================================
% 3-D: only one manual X or Y range restricts the others.
cla(ax, 'reset');
plot3(ax, 1:10, 1:10, (1:10) * 10);
set(ax, 'XLim', [2 3]);
drawnow();
assert_isapprox(ax.YLim, [2 3], 1e-12);
assert_isapprox(ax.ZLim, [20 30], 1e-12);
set(ax, 'YLim', [2 3]);
drawnow();
assert_isapprox(ax.ZLim, [0 100], 1e-12);
cla(ax, 'reset');
plot3(ax, 1:10, 1:10, (1:10) * 10);
set(ax, 'ZLim', [20 30]);
drawnow();
assert_isapprox(ax.XLim, [0 10], 1e-12);
assert_isapprox(ax.YLim, [0 10], 1e-12);
close(f);
%=============================================================================
