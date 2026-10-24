%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Present six plot types commonly used to explore numerical results.
if ~exist('essentialPlotsFigureVisible', 'var')
  essentialPlotsFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Essential plot types', ...
  'NumberTitle', 'off', ...
  'Visible', essentialPlotsFigureVisible);

curveX = linspace(0, 2 * pi, 120);
lineAxes = subplot(2, 3, 1, 'Parent', figureHandle);
plot(lineAxes, curveX, sin(curveX), 'LineWidth', 1.6);
hold(lineAxes, 'on');
plot(lineAxes, curveX, cos(curveX), '--', 'LineWidth', 1.4);
hold(lineAxes, 'off');
grid(lineAxes, 'on');
title(lineAxes, 'Line series');
legend(lineAxes, 'sin(x)', 'cos(x)', 'Location', 'best');

measurementX = 1:8;
measurementY = [1.1 1.8 2.6 3.0 4.2 4.8 5.9 6.4];
uncertainty = [0.20 0.25 0.18 0.30 0.22 0.28 0.24 0.20];
errorAxes = subplot(2, 3, 2, 'Parent', figureHandle);
errorbar(errorAxes, measurementX, measurementY, uncertainty, 'o-');
grid(errorAxes, 'on');
title(errorAxes, 'Measurements and error');

categoryValues = [4 7 5; 6 3 8; 5 6 4; 8 5 7];
barAxes = subplot(2, 3, 3, 'Parent', figureHandle);
bar(barAxes, categoryValues, 'grouped');
set(barAxes, 'XLim', [0.511111111111111 4.48888888888889], ...
  'YLim', [0 8], 'YTick', 0:1:8);
title(barAxes, 'Grouped bars');

distributionValues = [...
  -1.8 - 1.4 - 1.2 - 0.9 - 0.8 - 0.6 - 0.5 - 0.3 - 0.2 - 0.1 ...
  0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.8 0.9 1.1 1.4 1.7];
histogramAxes = subplot(2, 3, 4, 'Parent', figureHandle);
histogram(histogramAxes, distributionValues, 8);
set(histogramAxes, 'XLim', [-8.52 2.92], 'YLim', [0 10], ...
  'ZLim', [-1 1], 'XTick', -8:2:2, 'YTick', 0:2:10, 'ZTick', [-1 0 1]);
title(histogramAxes, 'Histogram');

[surfaceX, surfaceY] = meshgrid(linspace(-2.5, 2.5, 45));
surfaceZ = sin(surfaceX) .* cos(surfaceY) .* exp(-0.08 * ...
  (surfaceX .^ 2 + surfaceY .^ 2));
contourAxes = subplot(2, 3, 5, 'Parent', figureHandle);
contourf(contourAxes, surfaceX, surfaceY, surfaceZ, 12);
axis(contourAxes, 'equal');
set(contourAxes, 'XLim', [-2.5 2.5], 'YLim', [-2.5 2.5], ...
  'ZLim', [-1 1], 'XTick', -2:1:2, 'YTick', -2:1:2, 'ZTick', [-1 0 1]);
title(contourAxes, 'Filled contours');

surfaceAxes = subplot(2, 3, 6, 'Parent', figureHandle);
surf(surfaceAxes, surfaceX, surfaceY, surfaceZ, 'EdgeColor', 'none');
view(surfaceAxes, 3);
set(surfaceAxes, 'XLim', [-2.5 2.5], 'YLim', [-2.5 2.5], ...
  'ZLim', [-1 1], 'XTick', [-2 0 2], 'YTick', [-2 0 2], 'ZTick', -1:0.5:1);
grid(surfaceAxes, 'on');
title(surfaceAxes, 'Surface');
colormap(figureHandle, parula(128));
%=============================================================================
