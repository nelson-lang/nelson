%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compare one-dimensional interpolation methods on sparse samples.
sampleX = 0:1:10;
sampleY = sin(2 * pi * sampleX / 5);
queryX = 0:0.05:10;
referenceY = sin(2 * pi * queryX / 5);
figureHandle = figure('Name', 'Interpolation methods', 'NumberTitle', 'off');

nearestAxes = subplot(2, 2, 1, 'Parent', figureHandle);
plot(nearestAxes, queryX, referenceY, 'Color', [0.65 0.65 0.65], ...
  'LineWidth', 2);
hold(nearestAxes, 'on');
plot(nearestAxes, queryX, interp1(sampleX, sampleY, queryX, 'nearest'), ...
  'b-', 'LineWidth', 1.4);
plot(nearestAxes, sampleX, sampleY, 'ro', 'MarkerFaceColor', 'r');
hold(nearestAxes, 'off');
grid(nearestAxes, 'on');
title(nearestAxes, 'nearest');
xlabel(nearestAxes, 'x');
ylabel(nearestAxes, 'y');

linearAxes = subplot(2, 2, 2, 'Parent', figureHandle);
plot(linearAxes, queryX, referenceY, 'Color', [0.65 0.65 0.65], ...
  'LineWidth', 2);
hold(linearAxes, 'on');
plot(linearAxes, queryX, interp1(sampleX, sampleY, queryX, 'linear'), ...
  'b-', 'LineWidth', 1.4);
plot(linearAxes, sampleX, sampleY, 'ro', 'MarkerFaceColor', 'r');
hold(linearAxes, 'off');
grid(linearAxes, 'on');
title(linearAxes, 'linear');
xlabel(linearAxes, 'x');
ylabel(linearAxes, 'y');

pchipAxes = subplot(2, 2, 3, 'Parent', figureHandle);
plot(pchipAxes, queryX, referenceY, 'Color', [0.65 0.65 0.65], ...
  'LineWidth', 2);
hold(pchipAxes, 'on');
plot(pchipAxes, queryX, interp1(sampleX, sampleY, queryX, 'pchip'), ...
  'b-', 'LineWidth', 1.4);
plot(pchipAxes, sampleX, sampleY, 'ro', 'MarkerFaceColor', 'r');
hold(pchipAxes, 'off');
grid(pchipAxes, 'on');
title(pchipAxes, 'pchip');
xlabel(pchipAxes, 'x');
ylabel(pchipAxes, 'y');

splineAxes = subplot(2, 2, 4, 'Parent', figureHandle);
plot(splineAxes, queryX, referenceY, 'Color', [0.65 0.65 0.65], ...
  'LineWidth', 2);
hold(splineAxes, 'on');
plot(splineAxes, queryX, interp1(sampleX, sampleY, queryX, 'spline'), ...
  'b-', 'LineWidth', 1.4);
plot(splineAxes, sampleX, sampleY, 'ro', 'MarkerFaceColor', 'r');
hold(splineAxes, 'off');
grid(splineAxes, 'on');
title(splineAxes, 'spline');
xlabel(splineAxes, 'x');
ylabel(splineAxes, 'y');
%=============================================================================
