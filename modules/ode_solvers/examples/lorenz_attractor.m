%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Integrate the Lorenz system and display its phase-space trajectory.
initialState = [-3; -6; 12];
if ~exist('lorenzTimeSpan', 'var')
  lorenzTimeSpan = 0:0.01:20;
end
options = odeset(...
  'RelTol', 1e-6, ...
  'AbsTol', 1e-8);
[timeValues, stateValues] = ode45(@lorenzAttractorRhs, lorenzTimeSpan, initialState, options);

if ~exist('lorenzFigureVisible', 'var')
  lorenzFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Lorenz attractor', ...
  'NumberTitle', 'off', ...
  'Visible', lorenzFigureVisible);
axesHandle = axes('Parent', figureHandle);
plot3(axesHandle, stateValues(:, 1), stateValues(:, 2), stateValues(:, 3), ...
  'Color', [0.15 0.35 0.75], 'LineWidth', 1);
grid(axesHandle, 'on');
xlabel(axesHandle, 'x');
ylabel(axesHandle, 'y');
zlabel(axesHandle, 'z');
title(axesHandle, sprintf('Lorenz attractor over %.0f time units', timeValues(end)));
view(axesHandle, 3);
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function derivative = lorenzAttractorRhs(~, currentState)
  sigma = 10;
  rho = 28;
  beta = 8 / 3;
  derivative = [...
    sigma * (currentState(2) - currentState(1)); ...
    currentState(1) * (rho - currentState(3)) - currentState(2); ...
    currentState(1) * currentState(2) - beta * currentState(3)];
end
%=============================================================================
