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
[x, y] = meshgrid(0:2, 10:12);
u = [1 0 -1; 0 1 0; -1 0 1];
v = [0 1 0; 1 0 -1; 0 -1 0];
h = quiver(x, y, u, v, 0);
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
asserts.isequal(h.UData, u);
asserts.isequal(h.VData, v);
asserts.isequal(char(h.AutoScale), 'off');
asserts.isapprox(h.ScaleFactor, 1, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
group = hggroup('Parent', ax);
h = quiver(group, [1 2], [3 4], [0.5 -0.5], [1 -1], 'Color', 'k');
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.Parent, group);
asserts.isequal(ancestor(h, 'axes'), ax);
asserts.isequal(h.Color, [0 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = 1:5;
y = [1 2 1 2 1];
u = [1 0 -1 0 1];
v = [0 1 0 -1 0];
h = quiver(x, y, u, v, 0, 'r--o', 'LineWidth', 1.5);
h.ShowArrowHead = 'on';
ax = ancestor(h, 'axes');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.MarkerEdgeColor, 'auto');
asserts.isequal(h.LineStyle, '--');
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.DataLimits(1:4), [1 6 0.5 3]);
asserts.isequal(ax.XLim, [1 6]);
asserts.isequal(ax.YLim, [0.5 3]);
asserts.isequal(ax.XTick, 1:0.5:6);
asserts.isequal(ax.YTick, 0.5:0.5:3);
close(f);
%=============================================================================
asserts.checkerror('quiver(1);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('quiver({1}, {2});', getString(message('Nelson:notEnoughInputs')));
asserts.checkerror('quiver(1:2, 1:2, 1:2, 1:3);', getString(message('Nelson:dimagree:vectorComponentSizesMatch')));
asserts.checkerror('quiver(1:2, 1:3, 1:2, 1:2);', getString(message('Nelson:graphics:componentDataSizeMismatch', 'Y')));
asserts.checkerror('quiver(1:2, 1:2, 1:2, 1:2, ''LineWidth'');', getString(message('Nelson:graphics:nameValueArgumentsExpected')));
%=============================================================================
% the 'filled' flag is accepted (fills arrow markers -> MarkerFaceColor 'auto')
f = figure('Visible', 'off');
h = quiver(0, 0, 1, 1, 0, 'filled');
asserts.isequal(char(h.MarkerFaceColor), 'auto');
h2 = quiver(0, 0, 1, 1, 0, "filled", "Color", [1 0 0], "LineWidth", 2);
asserts.isequal(h2.Color, [1 0 0]);
asserts.isequal(char(h2.MarkerFaceColor), 'auto');
close(f);
%=============================================================================
