%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
reset(groot());
%=============================================================================
geometryTolerance = 1e-6;
titleTolerance = 1e-3;
%=============================================================================
figureInnerPosition = [100 100 560 420];
f = createVisualFigure(figureInnerPosition);
ax = axes('Parent', f);
plot(ax, 1:10, sin(1:10), 'r-', 'LineWidth', 1.5);
hold(ax, 'on');
plot(ax, 1:10, cos(1:10), 'b--', 'LineWidth', 1.5);
grid(ax, 'on');
title(ax, 'hold state');
xlabel(ax, 'x');
ylabel(ax, 'value');
frame = getframe(f);
assertFrameSize(f, frame);
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.815], geometryTolerance);
asserts.isapprox(ax.XLim, [1 10], geometryTolerance);
asserts.isapprox(ax.XTick, 1:10, geometryTolerance);
asserts.isapprox(ax.YLim, [-1 1], geometryTolerance);
asserts.isapprox(ax.YTick, -1:0.2:1, geometryTolerance);
asserts.isapprox(ax.CameraPosition, [5.5 0 17.3205080756888], geometryTolerance);
asserts.isapprox(ax.CameraTarget, [5.5 0 0], geometryTolerance);
asserts.isapprox(ax.CameraViewAngle, 6.60861036031192, geometryTolerance);
asserts.isequal(ax.GridLineStyle, '-');
close(f);
%=============================================================================
f = createVisualFigure(figureInnerPosition);
ax = axes('Parent', f);
imagesc(magic(12));
axis(ax, 'image');
frame = getframe(f);
assertFrameSize(f, frame);
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.815], geometryTolerance);
asserts.isapprox(ax.XLim, [0.5 12.5], geometryTolerance);
asserts.isapprox(ax.YLim, [0.5 12.5], geometryTolerance);
close(f);
%=============================================================================
f = createVisualFigure(figureInnerPosition);
ax = axes('Parent', f);
[x, y, z] = peaks(24);
surf(ax, x, y, z);
view(ax, 35, 28);
title(ax, 'surface view');
drawnow();
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.815], geometryTolerance);
asserts.isapprox(ax.XLim, [-4 4], geometryTolerance);
asserts.isapprox(ax.XTick, -4:2:4, geometryTolerance);
asserts.isapprox(ax.YLim, [-4 4], geometryTolerance);
asserts.isapprox(ax.YTick, -4:2:4, geometryTolerance);
asserts.isapprox(ax.ZLim, [-8 8], geometryTolerance);
asserts.isapprox(ax.ZTick, -8:2:8, geometryTolerance);
close(f);
%=============================================================================
f = figure('Visible', 'off');
axis manual
[X, Y] = meshgrid(linspace(-2, 2, 50));
Z = sin(2 * pi * (X .^ 2 + Y .^ 2));
h = surf(X, Y, Z);
colormap(jet);
colorbar;
t = title('Animated Surface Plot');
view([-29 82]);
drawnow();
frame = getframe(f);
assertFrameSize(f, frame);
ax = ancestor(h, 'axes');
asserts.isapprox(ax.XLim, [-2 2], geometryTolerance);
asserts.isapprox(ax.YLim, [-2 2], geometryTolerance);
asserts.isapprox(ax.ZLim, [-1 1], geometryTolerance);
asserts.isequal(t.Units, 'data');
asserts.isapprox(t.Position, [1.44111722097306 2.59984428757944 0.187535122352313], titleTolerance);
close(f);
%=============================================================================
f = createVisualFigure(figureInnerPosition);
t = tiledlayout(f, 1, 2);
ax1 = nexttile(t);
plot(ax1, 1:8, (1:8) .^ 2, 'k-o');
title(ax1, 'quadratic');
ax2 = nexttile(t);
bar(ax2, [3 5 2 7]);
title(ax2, 'bars');
drawnow();
asserts.isapprox(ax1.Position, [0.13 0.11 0.3175 0.815], geometryTolerance);
asserts.isapprox(ax1.FontSize, 9, geometryTolerance);
asserts.isapprox(ax2.Position, [0.5875 0.11 0.3175 0.815], geometryTolerance);
asserts.isapprox(ax2.FontSize, 9, geometryTolerance);
asserts.isapprox(ax1.XLim, [0 8], geometryTolerance);
asserts.isapprox(ax1.YLim, [0 70], geometryTolerance);
close(f);
%=============================================================================
function f = createVisualFigure(innerPosition)
  f = figure('Visible', 'off', 'InnerPosition', innerPosition, 'Color', [1 1 1]);
  asserts.isequal(f.InnerPosition, innerPosition);
end
%=============================================================================
function assertFrameSize(f, frame)
  pos = f.Position;
  % getframe captures a figure that contains axes at device resolution.
  ratio = f.DevicePixelRatio;
  asserts.isequal(size(frame.cdata), [round(pos(4) * ratio), round(pos(3) * ratio), 3]);
end
%=============================================================================
