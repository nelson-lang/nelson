%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Measure the convergence of ode45 on an exponential decay problem.
tolerances = logspace(-3, -8, 6);
meanSteps = zeros(size(tolerances));
maximumErrors = zeros(size(tolerances));

for toleranceIndex = 1:numel(tolerances)
  options = odeset('RelTol', tolerances(toleranceIndex), ...
    'AbsTol', tolerances(toleranceIndex));
  [time, solution] = ode45(@(t, y) - y, [0 5], 1, options);
  meanSteps(toleranceIndex) = 5 / (numel(time) - 1);
  maximumErrors(toleranceIndex) = max(abs(solution - exp(-time)));
end

referenceFourthOrder = maximumErrors(end) * ...
  (meanSteps / meanSteps(end)) .^ 4;
referenceFifthOrder = maximumErrors(end) * ...
  (meanSteps / meanSteps(end)) .^ 5;

figureHandle = figure('Name', 'ode45 convergence', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
loglog(axesHandle, meanSteps, maximumErrors, 'bo-', 'LineWidth', 1.5, ...
  'MarkerFaceColor', 'b');
hold(axesHandle, 'on');
loglog(axesHandle, meanSteps, referenceFourthOrder, '--', 'LineWidth', 1.2);
loglog(axesHandle, meanSteps, referenceFifthOrder, ':', 'LineWidth', 1.5);
hold(axesHandle, 'off');
grid(axesHandle, 'on');
xlabel(axesHandle, 'Mean integration step');
ylabel(axesHandle, 'Maximum absolute error');
title(axesHandle, 'Convergence on y'' = -y');
legend(axesHandle, 'measured', 'fourth order', 'fifth order', ...
  'Location', 'best');
%=============================================================================
