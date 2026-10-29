%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Constant-delay equation by the method of steps.
%
% This script solves
%
%     y'(t) = alpha * y(t - tau)
%
% with history y(t) = 1 for t <= 0. It uses ode45 on consecutive intervals.
% A finite-difference check estimates the sensitivity of the final value with
% respect to alpha without requiring a dedicated delay-equation API.
%=============================================================================

alpha = 2.0;
delayTime = 0.1;
historyValue = 1.0;
finalTime = 0.3;

[time, value] = solveDelayBySteps(alpha, delayTime, historyValue, finalTime);
finalValue = value(length(value));

perturbation = 1e-5;
[~, perturbedValue] = solveDelayBySteps(...
  alpha + perturbation, delayTime, historyValue, finalTime);
finiteDifferenceSensitivity = ...
  (perturbedValue(length(perturbedValue)) - finalValue) / perturbation;

disp('Constant-delay equation by method of steps');
disp(['  final value:              ', num2str(finalValue)]);
disp(['  finite-difference check:  ', num2str(finiteDifferenceSensitivity)]);

if abs(finalValue - 1.681333333333333) >= 5e-4
  error('Delay solution is outside tolerance.');
end
if abs(finiteDifferenceSensitivity - 0.382) >= 5e-3
  error('Delay sensitivity check is outside tolerance.');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

function [time, value] = solveDelayBySteps(alpha, delayTime, historyValue, finalTime)
  options = odeset('RelTol', 1e-8, 'AbsTol', 1e-10, 'MaxStep', delayTime / 10);
  time = 0;
  value = historyValue;
  intervalStart = 0;
  currentValue = historyValue;

  while intervalStart < finalTime
    intervalEnd = min(intervalStart + delayTime, finalTime);
    rhs = @(t, y) alpha * delayedState(t - delayTime, time, value, historyValue);
    segment = ode45(rhs, [intervalStart intervalEnd], currentValue, options);
    segmentTime = segment.x(:);
    segmentValue = segment.y(:);
    time = [time; segmentTime(2:length(segmentTime))];
    value = [value; segmentValue(2:length(segmentValue))];
    currentValue = value(length(value));
    intervalStart = intervalEnd;
  end
end

function ylag = delayedState(queryTime, time, value, historyValue)
  if queryTime <= 0
    ylag = historyValue;
  else
    ylag = interp1(time, value, queryTime, 'linear');
  end
end
%=============================================================================
