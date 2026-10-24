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
u = [1 0; 0 1];
v = [0 1; 1 0];
h = quiver(u, v);
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.Type, 'quiver');
asserts.isequal(numel(h.Children), 0);
asserts.isequal(h.XData, [1 2]);
asserts.isequal(h.YData, [1 2]);
asserts.isequal(h.UData, u);
asserts.isequal(h.VData, v);
asserts.isequal(h.WData, []);
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.YDataMode, 'auto');
asserts.isequal(char(h.AutoScale), 'on');
asserts.isapprox(h.AutoScaleFactor, 0.9, 1e-12);
asserts.isequal(h.Alignment, 'tail');
asserts.isequal(char(h.ShowArrowHead), 'on');
asserts.isequal(h.MaxHeadSize, 0.2);
ax = ancestor(h, 'axes');
asserts.isequal(h.Parent, ax);
asserts.isequal(ax.View, [0 90]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
x = [1 2 3];
y = [4 5 6];
u = [1 0 -1];
v = [0 1 0];
h = quiver(ax, x, y, u, v, 0.5, 'r--o', 'LineWidth', 2, ...
  'DisplayName', 'field');
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.Parent, ax);
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
asserts.isequal(h.UData, u);
asserts.isequal(h.VData, v);
asserts.isequal(h.XDataMode, 'manual');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(char(h.AutoScale), 'on');
asserts.isapprox(h.AutoScaleFactor, 0.5, 1e-12);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.DisplayName, 'field');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = quiver([1 0], [0 1], 'AutoScale', 'off', 'Alignment', 'center', ...
  'ShowArrowHead', 'off', 'Color', [0.2 0.3 0.4], 'MarkerFaceColor', 'none');
asserts.isequal(char(h.AutoScale), 'off');
asserts.isequal(h.Alignment, 'center');
asserts.isequal(char(h.ShowArrowHead), 'off');
asserts.isequal(h.Color, [0.2 0.3 0.4]);
asserts.isequal(h.MarkerFaceColor, 'none');
close(f);
%=============================================================================
% axis equal keeps quiver data limits and uses the standard 2-D Z frame.
f = figure('Visible', 'off');
[X, Y] = meshgrid(-2:0.5:2, -2:0.5:2);
U = -Y;
V = X;
quiver(X, Y, U, V);
axis equal
ax = gca();
asserts.isapprox(ax.XLim, [-3.04294478527607, 3.04294478527607], 1e-12);
asserts.isapprox(ax.YLim, [-2.4, 2.4], 1e-12);
asserts.isapprox(ax.ZLim, [-0.96, 0.96], 1e-12);
asserts.isequal(ax.ZTick, [-0.5, 0, 0.5]);
close(f);
%=============================================================================
asserts.istrue(isValidGraphicsProperty('quiver', 'UData'));
asserts.istrue(isValidGraphicsProperty('quiver', 'WData'));
asserts.istrue(isValidGraphicsProperty('quiver', 'ScaleFactor'));
asserts.istrue(isValidGraphicsProperty('quiver', 'ShowArrowHead'));
asserts.isfalse(isValidGraphicsProperty('quiver', 'Values'));
%=============================================================================
