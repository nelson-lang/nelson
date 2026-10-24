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
f = figure();
x = -pi:pi / 10:pi;
y = tan(sin(x)) - sin(tan(x));
L = plot(x, y, '--rs', 'LineWidth', 2, 'MarkerEdgeColor', 'k', 'MarkerFaceColor', 'g', 'MarkerSize', 11);
ax = gca();
asserts.isequal(ax.Position, [0.13 0.11 0.775 0.815]);
asserts.isequal(ax.FontSize, 10);
asserts.isequal(ax.LineWidth, 0.5);
asserts.isequal(ax.TickLength, [0.01 0.025]);
asserts.isequal(ax.XLim, [-4 4]);
asserts.isequal(ax.YLim, [-3 3]);
asserts.isequal(ax.XTick, -4:4);
asserts.isequal(ax.YTick, -3:3);
asserts.isequal(L.LineStyle, '--');
asserts.isequal(L.Color, [1 0 0]);
asserts.isequal(L.LineWidth, 2);
asserts.isequal(L.Marker, 'square');
asserts.isequal(L.MarkerEdgeColor, [0 0 0]);
asserts.isequal(L.MarkerFaceColor, [0 1 0]);
asserts.isequal(L.MarkerSize, 11);
%=============================================================================
f = figure();
x = linspace(-2 * pi, 2 * pi);
y1 = sin(x);
y2 = cos(x);
L = plot(x, y1, x, y2);
L(1).LineWidth = 2;
L(2).Marker = '*';
%=============================================================================
f = figure();
r = 2;
xc = 4;
yc = 3;
theta = linspace(0, 2 * pi);
x = r * cos(theta) + xc;
y = r * sin(theta) + yc;
plot(x, y);
axis equal
close all;
%=============================================================================
