%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Estimate a rotated ellipse from noisy observations.
rng(42);
sampleAngles = linspace(0, 2 * pi, 90).';
trueAxes = [3.2 1.4];
trueCenter = [1.1 -0.7];
trueAngle = pi / 5;
rotation = [cos(trueAngle) -sin(trueAngle); ...
  sin(trueAngle) cos(trueAngle)];
cleanPoints = ([trueAxes(1) * cos(sampleAngles), ...
  trueAxes(2) * sin(sampleAngles)] * rotation.') + trueCenter;
observedX = cleanPoints(:, 1) + 0.08 * randn(size(sampleAngles));
observedY = cleanPoints(:, 2) + 0.08 * randn(size(sampleAngles));

objective = @(parameters) sum((...
  ((cos(parameters(5)) * (observedX - parameters(3)) + ...
  sin(parameters(5)) * (observedY - parameters(4))) / exp(parameters(1))) .^ 2 + ...
  ((-sin(parameters(5)) * (observedX - parameters(3)) + ...
  cos(parameters(5)) * (observedY - parameters(4))) / exp(parameters(2))) .^ 2 ...
  - 1) .^ 2);
initialParameters = [log(3); log(1); mean(observedX); mean(observedY); 0];
[fittedParameters, residual] = fminsearch(objective, initialParameters, ...
  optimset('Display', 'off', 'MaxIter', 1200, 'MaxFunEvals', 2500));

fittedAxes = exp(fittedParameters(1:2));
fittedCenter = fittedParameters(3:4).';
fittedAngle = fittedParameters(5);
fittedRotation = [cos(fittedAngle) -sin(fittedAngle); ...
  sin(fittedAngle) cos(fittedAngle)];
fittedPoints = ([fittedAxes(1) * cos(sampleAngles), ...
  fittedAxes(2) * sin(sampleAngles)] * fittedRotation.') + fittedCenter;

figureHandle = figure('Name', 'Noisy ellipse fit', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
plot(axesHandle, observedX, observedY, '.', 'Color', [0.30 0.30 0.30]);
hold(axesHandle, 'on');
plot(axesHandle, cleanPoints(:, 1), cleanPoints(:, 2), '--', ...
  'Color', [0.20 0.60 0.25], 'LineWidth', 1.4);
plot(axesHandle, fittedPoints(:, 1), fittedPoints(:, 2), 'r-', ...
  'LineWidth', 1.8);
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
grid(axesHandle, 'on');
xlabel(axesHandle, 'x');
ylabel(axesHandle, 'y');
title(axesHandle, sprintf('Ellipse fit, residual %.3f', residual));
legend(axesHandle, 'observations', 'source ellipse', 'fitted ellipse', ...
  'Location', 'best');
%=============================================================================
