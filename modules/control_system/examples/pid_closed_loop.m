%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Design a filtered PID controller and inspect the closed-loop step response.
plant = tf(1, [1 3 2]);
proportionalGain = 8;
integralGain = 4;
derivativeGain = 1.5;
derivativeFilter = 20;
controllerNumerator = [...
  proportionalGain + derivativeGain * derivativeFilter, ...
  proportionalGain * derivativeFilter + integralGain, ...
  integralGain * derivativeFilter];
controllerDenominator = [1 derivativeFilter 0];
controller = tf(controllerNumerator, controllerDenominator);
openLoop = series(controller, plant);
closedLoop = feedback(openLoop, 1);
[responseValues, responseTimes] = step(closedLoop, [0 8]);
closedLoopPoles = pole(closedLoop);

if ~exist('pidExampleFigureVisible', 'var')
  pidExampleFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Filtered PID closed loop', ...
  'NumberTitle', 'off', ...
  'Visible', pidExampleFigureVisible);
axesHandle = axes('Parent', figureHandle);
plot(axesHandle, responseTimes, responseValues, ...
  'Color', [0.15 0.40 0.75], 'LineWidth', 1.8);
hold(axesHandle, 'on');
plot(axesHandle, responseTimes, ones(size(responseTimes)), '--', ...
  'Color', [0.35 0.35 0.35]);
hold(axesHandle, 'off');
grid(axesHandle, 'on');
xlabel(axesHandle, 'Time (s)');
ylabel(axesHandle, 'Output');
title(axesHandle, 'Closed-loop response with filtered PID control');
legend(axesHandle, 'response', 'setpoint', 'Location', 'best');
%=============================================================================
