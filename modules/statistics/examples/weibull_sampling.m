%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generate Weibull samples with inverse transform sampling.
rng(42);
sampleCount = 10000;
scale = 2;
shape = 1.5;
uniformSamples = rand(sampleCount, 1);
samples = scale * (-log(1 - uniformSamples)) .^ (1 / shape);

binEdges = linspace(0, 8, 33);
binCenters = (binEdges(1:end - 1) + binEdges(2:end)) / 2;
expectedProbability = ...
  exp(-(binEdges(1:end - 1) / scale) .^ shape) - ...
  exp(-(binEdges(2:end) / scale) .^ shape);

figureHandle = figure('Name', 'Weibull sampling', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
histogram(axesHandle, samples, binEdges, 'Normalization', 'probability', ...
  'FaceColor', [0.35 0.60 0.85]);
hold(axesHandle, 'on');
plot(axesHandle, binCenters, expectedProbability, 'r-', 'LineWidth', 2);
hold(axesHandle, 'off');
grid(axesHandle, 'on');
xlabel(axesHandle, 'Value');
ylabel(axesHandle, 'Probability per bin');
title(axesHandle, sprintf('Weibull samples: scale %.1f, shape %.1f', ...
  scale, shape));
legend(axesHandle, 'samples', 'expected probability', 'Location', 'best');
%=============================================================================
