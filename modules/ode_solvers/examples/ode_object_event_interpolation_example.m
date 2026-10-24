%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%=============================================================================
% Event detection and dense output interpolation with ode45.
%
% This script solves a logistic growth model until the population reaches a
% chosen threshold. It uses only the historical ODE function interface:
%   - odeset for Events and tolerances,
%   - ode45 with a solution structure output,
%   - deval for interpolation.
%=============================================================================

growthRate = 1.8;
carryingCapacity = 1.0;
initialPopulation = 0.05;
stopPopulation = 0.75;

options = odeset(...
  'Events', @(t, y) logisticEvent(t, y, stopPopulation), ...
  'RelTol', 1e-8, ...
  'AbsTol', 1e-10);

solution = ode45(...
  @(t, y) logisticRhs(t, y, growthRate, carryingCapacity), ...
  [0 5], ...
  initialPopulation, ...
  options);

eventTime = solution.xe(1);
eventValue = solution.ye(1);
queryTimes = linspace(0, eventTime, 6);
interpolatedPopulation = deval(solution, queryTimes);

expectedEventValue = logisticExact(eventTime, initialPopulation, growthRate, carryingCapacity);
eventError = abs(eventValue - expectedEventValue);

disp('Event detection and interpolation');
disp(['  event time:          ', num2str(eventTime)]);
disp(['  event value:         ', num2str(eventValue)]);
disp(['  event value error:   ', num2str(eventError)]);
disp('  interpolated values:');
disp(interpolatedPopulation);

if eventError >= 1e-3
  error('Event value is outside tolerance.');
end
if abs(eventValue - stopPopulation) >= 1e-6
  error('Event threshold was not reached accurately.');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

function dydt = logisticRhs(t, y, r, k)
  dydt = r * y .* (1 - y ./ k);
end

function [value, isTerminal, direction] = logisticEvent(t, y, threshold)
  value = y(1) - threshold;
  isTerminal = 1;
  direction = 1;
end

function y = logisticExact(t, y0, r, k)
  scale = (k - y0) ./ y0;
  y = k ./ (1 + scale .* exp(-r .* t));
end
%=============================================================================
