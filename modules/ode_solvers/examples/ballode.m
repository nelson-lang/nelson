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
% Bouncing ball simulation using ODE solver with event detection.
% Models a ball under gravity that bounces off the ground with energy loss.
% Optionally plots height in real time using odeplot as an output function.
%=============================================================================

% --- Simulation time parameters ---
simulationStartTime = 0; % Initial time of the simulation [seconds]
simulationEndTime = 30; % Final time of the simulation [seconds]

% --- Initial conditions: [position; velocity] ---
initialHeight = 0; % Ball starts at ground level [meters]
initialVelocity = 20; % Ball launched upward at 20 m/s
initialState = [initialHeight; initialVelocity];

% --- ODE solver refinement factor (extra interpolation points per step) ---
outputRefineFactor = 4;

% --- Toggle for using the live output plotting function ---
if exist('ballodeUseOutputFcn', 'var') == 0
  ballodeUseOutputFcn = true; % Default: enable real-time ODE plot
end

% --- Configure ODE solver options based on whether live plotting is enabled ---
if ballodeUseOutputFcn
  % With live plotting: attach event detector, output function, and refinement
  solverOptions = odeset(...
    'Events', @bounceEventFcn, ...  % Stop integration when ball hits ground
    'OutputFcn', @odeplot, ...         % Plot height in real time
    'OutputSel', 1, ...                % Plot only the first state variable (height)
    'Refine', outputRefineFactor, ...
    'MaxStep', 0.05); % Cap step size for smooth output
else
  % Without live plotting: event detection only
  solverOptions = odeset(...
    'Events', @bounceEventFcn, ...
    'Refine', outputRefineFactor, ...
    'MaxStep', 0.05);
end

% --- Figure visibility toggle (used for testing without displaying a window) ---
if exist('ballodeVisible', 'var') == 0
  ballodeVisible = 'on'; % Default: show the figure window
end

% --- Create figure and axes for final trajectory plot ---
trajectoryFig = figure('Visible', ballodeVisible);
trajectoryFig.UserData = struct('stop', false); % Used to support early stop from UI
trajectoryAxes = axes;
trajectoryAxes.XLim = [0 30]; % Full simulation time window [seconds]
trajectoryAxes.YLim = [0 25]; % Expected max height range [meters]
hold on

% --- Pre-allocate output arrays to accumulate results across bounces ---
timeOutput = simulationStartTime; % Accumulated time points
stateOutput = initialState.'; % Accumulated [height, velocity] rows
eventTimeOutput = []; % Times at which bounce events occurred
eventStateOutput = []; % States at bounce events
eventIndexOutput = []; % Index of triggered event (always 1 here)

% --- Main loop: integrate across successive bounces (up to 10 bounces) ---
for bounceCount = 1:10

  % Integrate the ODE from current start time to end of simulation
  [timeSegment, stateSegment, eventTimes, eventStates, eventIndices] = ...
    ode23(@ballDynamics, [simulationStartTime simulationEndTime], initialState, solverOptions);

  % Ensure hold is active for incremental plotting
  if ~ishold
    hold on
  end

  % Number of time points returned in this integration segment
  numTimePoints = length(timeSegment);

  % Append this segment's results (skip first point to avoid duplication)
  timeOutput = [timeOutput; timeSegment(2:numTimePoints)];
  stateOutput = [stateOutput; stateSegment(2:numTimePoints, :)];
  eventTimeOutput = [eventTimeOutput; eventTimes];
  eventStateOutput = [eventStateOutput; eventStates];
  eventIndexOutput = [eventIndexOutput; eventIndices];

  % Check if user has requested an early stop via figure UserData flag
  figureUserData = trajectoryFig.UserData;
  if isstruct(figureUserData) && isfield(figureUserData, 'stop') && figureUserData.stop
    break
  end

  % Stop if no bounce event occurred or simulation reached the final time
  if isempty(eventTimes) || timeSegment(numTimePoints) >= simulationEndTime
    break
  end

  % --- Apply bounce: reset position to ground, reverse and dampen velocity ---
  bounceDampingCoefficient = 0.9; % 10% energy loss per bounce
  lastEventRow = size(eventStates, 1); % Index of most recent event
  velocityAtBounce = eventStates(lastEventRow, 2); % Velocity just before impact
  initialState(1) = 0; % Ball is back at ground
  initialState(2) = -bounceDampingCoefficient * velocityAtBounce; % Reverse & dampen

  % Advance start time to where the ball hit the ground
  simulationStartTime = timeSegment(numTimePoints);
end

% --- Final plot: draw full trajectory or event markers ---
if ~ballodeUseOutputFcn
  % If live plotting was disabled, draw the full height trajectory now
  plot(timeOutput, stateOutput(:, 1))
end

% Mark each bounce event as a red circle on the plot
plot(eventTimeOutput, eventStateOutput(:, 1), 'ro')
xlabel('time');
ylabel('height');
title('Ball trajectory and the events');
hold off

% Signal odeplot that integration is complete (cleans up the live plot)
if ballodeUseOutputFcn
  odeplot([], [], 'done');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

%-----------------------------------------------------------------------------
% ballDynamics - ODE right-hand side for a ball in free fall under gravity.
%
%   Inputs:
%     currentTime  - current time (unused, but required by ODE solver API)
%     currentState - [height; velocity] at current time
%
%   Output:
%     stateDerivative - [velocity; acceleration] = [y(2); -g]
%-----------------------------------------------------------------------------
function stateDerivative = ballDynamics(currentTime, currentState)
  gravitationalAcceleration = -9.8; % [m/s^2], directed downward
  heightDerivative = currentState(2); % dh/dt = velocity
  velocityDerivative = gravitationalAcceleration; % dv/dt = -g
  stateDerivative = [heightDerivative; velocityDerivative];
end

%-----------------------------------------------------------------------------
% bounceEventFcn - Event function to detect when the ball hits the ground.
%
%   The ODE solver monitors 'value' and stops integration when it crosses
%   zero in the specified direction (downward crossing = ball falling to ground).
%
%   Inputs:
%     eventTime  - current time (unused)
%     eventState - [height; velocity] at current time
%
%   Outputs:
%     value       - quantity to monitor (height); zero crossing triggers event
%     isTerminal  - 1 = stop integration when event is detected
%     direction   - -1 = only trigger on downward zero crossings (falling ball)
%-----------------------------------------------------------------------------
function [value, isTerminal, direction] = bounceEventFcn(eventTime, eventState)
  value = eventState(1); % Monitor height: event fires when height = 0
  isTerminal = 1; % Stop the ODE solver at this event
  direction = -1; % Only detect the ball moving downward through zero
end
