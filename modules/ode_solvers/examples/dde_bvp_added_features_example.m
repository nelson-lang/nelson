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
% Delay and boundary value solver features.
%
% This script demonstrates the DDE and BVP entry points added to the ODE solver
% module. It is intentionally written with portable syntax and function calls.
%=============================================================================

timeSpan = [0 0.2];
constantDelaySolution = dde23(@delayRightHandSide, 0.1, 1, timeSpan);
stateDelay = @(t, y) t - 0.05 + 0 * y(1);
stateDelaySolution = ddesd(@delayRightHandSide, stateDelay, 1, timeSpan);
neutralSolution = ddensd(@neutralDelayRightHandSide, stateDelay, stateDelay, 1, timeSpan);

queryTime = linspace(timeSpan(1), timeSpan(2), 16);
constantDelayValues = deval(constantDelaySolution, queryTime);
stateDelayValues = deval(stateDelaySolution, queryTime);
neutralDelayValues = deval(neutralSolution, queryTime);

initialMesh = linspace(0, 1, 5);
initialGuess = bvpinit(initialMesh, @linearInitialGuess);
linear4 = bvp4c(@linearEquation, @linearBoundaryConditions, initialGuess);
linear5 = bvp5c(@linearEquation, @linearBoundaryConditions, initialGuess);
extendedLinear = bvpxtend(linear4, 1.25, [1.25; 1]);

space = linspace(0, 1, 24);
linear4Values = deval(linear4, space);
linear5Values = deval(linear5, space);

ddeError = max(abs(constantDelayValues - 1));
linearError = max(abs(linear4Values(1, :) - space));
linearDifference = max(abs(linear4Values(1, :) - linear5Values(1, :)));
extensionError = abs(extendedLinear.y(1, length(extendedLinear.x)) - 1.25);

disp('DDE and BVP added features');
disp(['  dde23 constant solution error: ', num2str(ddeError)]);
disp(['  bvp4c linear solution error:   ', num2str(linearError)]);
disp(['  bvp4c/bvp5c difference:        ', num2str(linearDifference)]);
disp(['  bvpxtend extension error:      ', num2str(extensionError)]);

if ddeError >= 1e-8
  error('DDE constant delay example is outside tolerance.');
end
if linearError >= 1e-8
  error('BVP linear solution is outside tolerance.');
end
if linearDifference >= 1e-8
  error('BVP solver comparison is outside tolerance.');
end
if extensionError >= 1e-8
  error('BVP extension is outside tolerance.');
end

if exist('ddeBvpAddedFeaturesPlotEnabled', 'var') == 0
  ddeBvpAddedFeaturesPlotEnabled = true;
end

if ddeBvpAddedFeaturesPlotEnabled && exist('figure') ~= 0
  if exist('ddeBvpAddedFeaturesPlotVisible', 'var') == 0
    ddeBvpAddedFeaturesPlotVisible = 'on';
  end
  figure('Visible', ddeBvpAddedFeaturesPlotVisible);
  subplot(2, 1, 1);
  plot(queryTime, constantDelayValues(1, :), '-', ...
    queryTime, stateDelayValues(1, :), '--', ...
    queryTime, neutralDelayValues(1, :), ':');
  grid on
  xlabel('time');
  ylabel('state');
  title('Delay equation solutions');
  legend('dde23', 'ddesd', 'ddensd');

  subplot(2, 1, 2);
  plot(space, linear4Values(1, :), '-', ...
    space, linear5Values(1, :), '--', ...
    extendedLinear.x, extendedLinear.y(1, :), ':');
  grid on
  xlabel('space');
  ylabel('state');
  title('Boundary value solutions');
  legend('bvp4c', 'bvp5c', 'bvpxtend guess');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

function dydt = delayRightHandSide(t, y, z)
  dydt = -y + z(:, 1) + 0 * t;
end
%=============================================================================

function dydt = neutralDelayRightHandSide(t, y, z, zp)
  dydt = -y + z(:, 1) + 0 * zp(:, 1) + 0 * t;
end
%=============================================================================

function y = linearInitialGuess(x)
  y = [x; 1 + 0 * x];
end
%=============================================================================

function dydx = linearEquation(x, y)
  dydx = [y(2); 0 * y(1) + 0 * x];
end
%=============================================================================

function residual = linearBoundaryConditions(ya, yb)
  residual = [ya(1); yb(1) - 1];
end
%=============================================================================
