%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Render the Mandelbrot set by recording escape iterations.
horizontalPixels = 600;
verticalPixels = 420;
maximumIterations = 90;
realAxis = linspace(-2.25, 0.8, horizontalPixels);
imaginaryAxis = linspace(-1.25, 1.25, verticalPixels);
[realGrid, imaginaryGrid] = meshgrid(realAxis, imaginaryAxis);
parameter = realGrid + 1i * imaginaryGrid;
orbit = zeros(size(parameter));
escapeIteration = zeros(size(parameter));
active = true(size(parameter));

for iteration = 1:maximumIterations
  orbit(active) = orbit(active) .^ 2 + parameter(active);
  escaped = active & abs(orbit) > 2;
  escapeIteration(escaped) = iteration;
  active(escaped) = false;
end
escapeIteration(active) = maximumIterations;

figureHandle = figure('Name', 'Mandelbrot fractal', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
image('Parent', axesHandle, 'CData', escapeIteration, ...
  'XData', [realAxis(1) realAxis(end)], ...
  'YData', [imaginaryAxis(1) imaginaryAxis(end)], ...
  'CDataMapping', 'scaled');
set(axesHandle, 'YDir', 'normal');
axis(axesHandle, 'image');
axis(axesHandle, 'off');
colormap(axesHandle, parula(256));
title(axesHandle, sprintf('Mandelbrot set, %d iterations', maximumIterations));
%=============================================================================
