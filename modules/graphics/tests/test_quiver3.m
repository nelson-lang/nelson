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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
f = figure('Visible', 'off');
z = [0 1; 1 0];
u = [1 0; 0 1];
v = [0 1; 1 0];
w = ones(2, 2);
h = quiver3(z, u, v, w, 0);
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.Type, 'quiver');
asserts.isequal(numel(h.Children), 0);
asserts.isequal(h.XData, [1 2]);
asserts.isequal(h.YData, [1 2]);
asserts.isequal(h.ZData, z);
asserts.isequal(h.UData, u);
asserts.isequal(h.VData, v);
asserts.isequal(h.WData, w);
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.YDataMode, 'auto');
asserts.isequal(char(h.AutoScale), 'off');
asserts.isapprox(h.ScaleFactor, 1, 1e-12);
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(char(ax.XGrid), 'on');
asserts.isequal(char(ax.YGrid), 'on');
asserts.isequal(char(ax.ZGrid), 'on');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
x = [1 2 3];
y = [4 5 6];
z = [7 8 9];
u = [1 0 0];
v = [0 1 0];
w = [0 0 1];
h = quiver3(ax, x, y, z, u, v, w, 0.5, 'r--o', 'LineWidth', 2, ...
  'MaxHeadSize', 0.5);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
asserts.isequal(h.ZData, z);
asserts.isequal(h.UData, u);
asserts.isequal(h.VData, v);
asserts.isequal(h.WData, w);
asserts.isequal(h.XDataMode, 'manual');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.Color, [1 0 0]);
asserts.isapprox(h.AutoScaleFactor, 0.5, 1e-12);
asserts.isapprox(h.MaxHeadSize, 0.5, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[x, y, z] = meshgrid(1:2, 1:2, 1:2);
h = quiver3(x, y, z, ones(size(x)), zeros(size(x)), zeros(size(x)));
asserts.istrue(isgraphics(h, 'quiver'));
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
asserts.isequal(h.ZData, z);
asserts.isequal(size(h.UData), size(x));
close(f);
%=============================================================================
f = figure('Visible', 'off');
[x, y, z] = meshgrid(-1:1, -1:1, -1:1);
u = y;
v = -x;
w = z;
h = quiver3(x, y, z, u, v, w);
axis equal;
ax = ancestor(h, 'axes');
asserts.isapprox(h.ScaleFactor, 0.382970843102535, 1e-12);
asserts.isapprox(ax.XLim, [-1.38297084310254 1.38297084310254], 1e-12);
asserts.isapprox(ax.YLim, [-1.38297084310254 1.38297084310254], 1e-12);
asserts.isapprox(ax.ZLim, [-1.38297084310254 1.38297084310254], 1e-12);
asserts.isequal(ax.ZTick, [-1 -0.5 0 0.5 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [0 1 2];
y = [0 1 0];
z = [0 0 1];
u = [1 0 -1];
v = [0 1 0];
w = [0.5 0.5 1];
h = quiver3(x, y, z, u, v, w, 0, 'r--o');
h.LineWidth = 1.5;
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0 2]);
asserts.isequal(ax.YLim, [0 2]);
asserts.isequal(ax.ZLim, [0 2]);
asserts.isequal(ax.XTick, [0 0.5 1 1.5 2]);
asserts.isequal(ax.YTick, [0 0.5 1 1.5 2]);
asserts.isapprox(ax.ZTick, 0:0.2:2, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[X, Y, Z] = sphere(10);
[U, V, W] = surfnorm(X, Y, Z);
h = quiver3(X, Y, Z, U, V, W, 0);
axis equal;
ax = ancestor(h, 'axes');
asserts.isapprox(ax.XLim, [-1.9986453807384628 2], 1e-12);
asserts.isapprox(ax.YLim, [-1.9021130325903073 1.9021130325903073], 1e-12);
asserts.isapprox(ax.ZLim, [-1.9021130325903073 1.9021130325903073], 1e-12);
asserts.isequal(ax.XLimMode, 'auto');
asserts.isequal(ax.YLimMode, 'auto');
asserts.isequal(ax.ZLimMode, 'auto');
close(f);
%=============================================================================
try
  quiver3(1, 2);
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:narginchk:notEnoughInputs');
end
try
  quiver3({1}, {2}, {3}, {4});
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:notEnoughInputs');
end
try
  quiver3(1:2, 1:3, 1:2, 1:2, 1:2, 1:2);
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:graphics:componentDataSizeMismatch');
end
try
  quiver3(1:2, 1:2, 1:2, 1:2, 1:3, 1:2);
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:dimagree:vectorComponentSizesMatch');
end
%=============================================================================
f = figure('Visible', 'off');
[X, Y] = meshgrid(-2:0.25:2, -1:0.2:1);
Z = X .* exp(-X .^ 2 - Y .^ 2);
[U, V, W] = surfnorm(X, Y, Z);
h = quiver3(X, Y, Z, U, V, W);
hold on;
surf(X, Y, Z);
axis equal;
ax = ancestor(h, 'axes');
asserts.istrue(h.ScaleFactor > 1e-3);
asserts.isapprox(ax.XLim, [-2.00420096319369 2.03202035872739], 1e-12);
asserts.isapprox(ax.YLim, [-1.08479009213937 1.08479009213937], 1e-12);
asserts.isapprox(ax.ZLim, [-0.427337118548192 0.698505436241602], 1e-12);
asserts.isequal(ax.XTick, -2:0.5:2);
asserts.isequal(ax.YTick, -1:0.5:1);
asserts.isapprox(ax.ZTick, -0.4:0.2:0.6, 1e-12);
close(f);
%=============================================================================
