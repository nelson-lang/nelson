%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Delayed logistic growth (dde23 / ddesd) settling on a limit cycle, and the
% two solutions of y'' + |y| = 0 found by bvp4c / bvp5c from different guesses.
%=============================================================================
growthRate = 1.8;
timeSpan = [0 30];
hutchinson = @(t, y, Z) growthRate * y * (1 - Z(1));
constantDelaySolution = dde23(hutchinson, 1, 0.5, timeSpan);
varyingDelay = @(t, y) t - (1 + 0.3 * sin(t));
varyingDelaySolution = ddesd(hutchinson, varyingDelay, 0.5, timeSpan);
queryTime = linspace(timeSpan(1), timeSpan(2), 400);
constantDelayValues = deval(constantDelaySolution, queryTime);
varyingDelayValues = deval(varyingDelaySolution, queryTime);

twoSolutionsOde = @(x, y) [y(2); -abs(y(1))];
twoSolutionsBc = @(ya, yb) [ya(1); yb(1) + 2];
initialMesh = linspace(0, 4, 5);
upperSolution = bvp4c(twoSolutionsOde, twoSolutionsBc, bvpinit(initialMesh, [1 0]));
lowerSolution = bvp4c(twoSolutionsOde, twoSolutionsBc, bvpinit(initialMesh, [-1 0]));
upperSolution5 = bvp5c(twoSolutionsOde, twoSolutionsBc, bvpinit(initialMesh, [1 0]));
space = linspace(0, 4, 100);
upperValues = deval(upperSolution, space);
lowerValues = deval(lowerSolution, space);
upperValues5 = deval(upperSolution5, space);

lateCycle = constantDelayValues(queryTime > 15);
disp('Delay and boundary value solvers');
disp(['  dde23 limit cycle range:      [', num2str(min(lateCycle), 3), ', ', ...
  num2str(max(lateCycle), 3), ']']);
disp(['  bvp4c initial slopes:         ', num2str(upperValues(2, 1), 4), ' and ', ...
  num2str(lowerValues(2, 1), 4)]);
disp(['  bvp4c / bvp5c max difference: ', ...
  num2str(max(abs(upperValues(1, :) - upperValues5(1, :))), 3)]);

if exist('ddeBvpAddedFeaturesPlotEnabled', 'var') == 0
  ddeBvpAddedFeaturesPlotEnabled = true;
end

if ddeBvpAddedFeaturesPlotEnabled && exist('figure') ~= 0
  if exist('ddeBvpAddedFeaturesPlotVisible', 'var') == 0
    ddeBvpAddedFeaturesPlotVisible = 'on';
  end
  figure('Visible', ddeBvpAddedFeaturesPlotVisible, ...
    'Name', 'Delay and boundary value solvers', 'NumberTitle', 'off');
  subplot(2, 1, 1);
  plot(queryTime, constantDelayValues(1, :), '-', ...
    queryTime, varyingDelayValues(1, :), '--', ...
    timeSpan, [1 1], 'k:', 'LineWidth', 1.2);
  grid on
  xlabel('time');
  ylabel('population y(t)');
  title(sprintf('Delayed logistic growth, r = %.1f', growthRate));
  legend('dde23, constant delay 1', 'ddesd, delay 1 + 0.3 sin(t)', 'carrying capacity');

  subplot(2, 1, 2);
  plot(space, upperValues(1, :), '-', space, lowerValues(1, :), '-', ...
    space, upperValues5(1, :), '--', 'LineWidth', 1.2);
  grid on
  xlabel('x');
  ylabel('y(x)');
  title('y" + abs(y) = 0, y(0) = 0, y(4) = -2: two solutions');
  legend('bvp4c, guess y = 1', 'bvp4c, guess y = -1', 'bvp5c, guess y = 1');
end
%=============================================================================
