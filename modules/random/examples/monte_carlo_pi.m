%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Approximate pi with uniformly distributed points in a square.
rng(42);
sampleCount = 20000;
points = 2 * rand(sampleCount, 2) - 1;
insideCircle = sum(points .^ 2, 2) <= 1;
piEstimate = 4 * sum(insideCircle) / sampleCount;

figureHandle = figure('Name', 'Monte Carlo estimate of pi', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
hold(axesHandle, 'on');
scatter(axesHandle, points(insideCircle, 1), points(insideCircle, 2), ...
  6, [0.20 0.45 0.85], 'filled');
scatter(axesHandle, points(~insideCircle, 1), points(~insideCircle, 2), ...
  6, [0.90 0.35 0.25], 'filled');

theta = linspace(0, 2 * pi, 360);
plot(axesHandle, cos(theta), sin(theta), 'k', 'LineWidth', 1.5);
axis(axesHandle, 'equal');
xlim(axesHandle, [-1 1]);
ylim(axesHandle, [-1 1]);
grid(axesHandle, 'on');
xlabel(axesHandle, 'x');
ylabel(axesHandle, 'y');
title(axesHandle, sprintf('pi approximately %.5f using %d points', ...
  piEstimate, sampleCount));
hold(axesHandle, 'off');

fprintf('Monte Carlo estimate of pi: %.8f\n', piEstimate);
%=============================================================================
