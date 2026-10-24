%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Simulate and animate three rigid rods connected by frictionless pivots.
if ~exist('triplePendulumFigureVisible', 'var')
  triplePendulumFigureVisible = 'on';
end
if ~exist('triplePendulumAnimationEnabled', 'var')
  triplePendulumAnimationEnabled = true;
end
if ~exist('triplePendulumAnimationFrames', 'var')
  triplePendulumAnimationFrames = 120;
end
if ~exist('triplePendulumFramePause', 'var')
  triplePendulumFramePause = 0;
end
if ~exist('triplePendulumAnimationContinuous', 'var')
  triplePendulumAnimationContinuous = ...
    strcmp(triplePendulumFigureVisible, 'on');
end
% Play the motion back in real time (simulated seconds match wall-clock
% seconds) when the figure is on screen; off-screen renders skip the pacing
% so batch tests stay fast.
if ~exist('triplePendulumRealTime', 'var')
  triplePendulumRealTime = strcmp(triplePendulumFigureVisible, 'on');
end
if ~exist('triplePendulumPlaybackSpeed', 'var')
  triplePendulumPlaybackSpeed = 1;
end

triplePendulumGravity = 9.81;
triplePendulumLengths = [1.00; 0.90; 0.80];
triplePendulumMasses = [1.00; 0.85; 0.70];
triplePendulumCenters = triplePendulumLengths / 2;
triplePendulumInertias = ...
  triplePendulumMasses .* triplePendulumLengths .^ 2 / 12;
triplePendulumCoupling = triplePendulumCouplingMatrix(...
  triplePendulumLengths, triplePendulumCenters, triplePendulumMasses);
triplePendulumGravityLever = triplePendulumGravityMoments(...
  triplePendulumLengths, triplePendulumCenters, triplePendulumMasses);

triplePendulumDuration = 6;
triplePendulumTimeRequested = linspace(0, triplePendulumDuration, 301);
triplePendulumInitialAngles = [96; 88; 93] * pi / 180;
triplePendulumInitialState = [triplePendulumInitialAngles; zeros(3, 1)];
triplePendulumOptions = odeset('RelTol', 1e-7, 'AbsTol', 1e-9, ...
  'MaxStep', 0.05);
triplePendulumFigure = figure('Name', 'Rigid triple pendulum', ...
  'NumberTitle', 'off', 'Color', [0.97 0.98 1.00], ...
  'Visible', triplePendulumFigureVisible, 'Position', [80 80 1200 720]);
triplePendulumLoadingAxes = axes('Parent', triplePendulumFigure, ...
  'Position', [0 0 1 1], 'Visible', 'off');
triplePendulumLoadingText = text(triplePendulumLoadingAxes, 0.5, 0.53, ...
  'Computing the nonlinear trajectory...', ...
  'HorizontalAlignment', 'center', 'FontSize', 18, ...
  'Color', [0.16 0.20 0.28]);
triplePendulumLoadingSubtitle = text(triplePendulumLoadingAxes, 0.5, 0.46, ...
  'Three distributed-mass rigid rods', ...
  'HorizontalAlignment', 'center', 'FontSize', 12, ...
  'Color', [0.40 0.44 0.52]);
drawnow();
[triplePendulumTime, triplePendulumState] = ode45(...
  @(time, state) triplePendulumRhs(time, state, ...
  triplePendulumGravity, triplePendulumCoupling, ...
  triplePendulumGravityLever, triplePendulumInertias), ...
  triplePendulumTimeRequested, triplePendulumInitialState, ...
  triplePendulumOptions);
triplePendulumAngles = triplePendulumState(:, 1:3);
triplePendulumAngularRates = triplePendulumState(:, 4:6);
triplePendulumEnergy = triplePendulumTotalEnergy(...
  triplePendulumAngles, triplePendulumAngularRates, ...
  triplePendulumGravity, triplePendulumCoupling, ...
  triplePendulumGravityLever, triplePendulumInertias);
triplePendulumEnergyDrift = ...
  (triplePendulumEnergy - triplePendulumEnergy(1)) / ...
  max(1, abs(triplePendulumEnergy(1)));

set(triplePendulumLoadingText, 'Visible', 'off');
set(triplePendulumLoadingSubtitle, 'Visible', 'off');
triplePendulumMotionAxes = subplot(2, 3, [1 2 4 5], ...
  'Parent', triplePendulumFigure);
hold(triplePendulumMotionAxes, 'on');
triplePendulumSpan = 1.12 * sum(triplePendulumLengths);
axis(triplePendulumMotionAxes, [-triplePendulumSpan triplePendulumSpan ...
  -triplePendulumSpan 0.55]);
axis(triplePendulumMotionAxes, 'equal');
grid(triplePendulumMotionAxes, 'on');
set(triplePendulumMotionAxes, 'Color', [0.985 0.990 1.000]);
xlabel(triplePendulumMotionAxes, 'Horizontal position (m)');
ylabel(triplePendulumMotionAxes, 'Vertical position (m)');
title(triplePendulumMotionAxes, 'Three coupled rigid rods');

triplePendulumColors = [0.10 0.38 0.78; ...
  0.93 0.39 0.12; 0.48 0.19 0.70];
[triplePendulumJointsInitial, triplePendulumCentersInitial] = ...
  triplePendulumGeometry(triplePendulumInitialAngles, ...
  triplePendulumLengths, triplePendulumCenters);
triplePendulumRodPatches = gobjects(1, 3);
for rodIndex = 1:3
  rodPolygon = triplePendulumRodPolygon(...
    triplePendulumJointsInitial(rodIndex, :), ...
    triplePendulumJointsInitial(rodIndex + 1, :), 0.075);
  triplePendulumRodPatches(rodIndex) = patch(...
    'Parent', triplePendulumMotionAxes, ...
    'XData', rodPolygon(:, 1), 'YData', rodPolygon(:, 2), ...
    'FaceColor', triplePendulumColors(rodIndex, :), ...
    'EdgeColor', [0.12 0.14 0.18], 'LineWidth', 1.2);
end
triplePendulumJointMarkers = plot(triplePendulumMotionAxes, ...
  triplePendulumJointsInitial(:, 1), triplePendulumJointsInitial(:, 2), ...
  'o', 'Color', [0.08 0.09 0.12], 'MarkerFaceColor', [0.96 0.78 0.18], ...
  'MarkerSize', 8, 'LineStyle', 'none');
triplePendulumCenterMarkers = plot(triplePendulumMotionAxes, ...
  triplePendulumCentersInitial(:, 1), triplePendulumCentersInitial(:, 2), ...
  '.', 'Color', [0.08 0.09 0.12], 'MarkerSize', 13, ...
  'LineStyle', 'none');
triplePendulumFixedSupport = plot(triplePendulumMotionAxes, ...
  [-0.20 0.20 NaN 0 0], [0.22 0.22 NaN 0.22 0], ...
  'Color', [0.08 0.09 0.12], 'LineWidth', 2.2);
triplePendulumFixedPivot = plot(triplePendulumMotionAxes, 0, 0, ...
  'o', 'Color', [0.08 0.09 0.12], 'MarkerFaceColor', [1.00 0.82 0.20], ...
  'MarkerSize', 10, 'LineWidth', 1.5);
triplePendulumTipTrail = animatedline(triplePendulumMotionAxes, ...
  'Color', [0.20 0.22 0.28], 'LineWidth', 1.2, 'MaximumNumPoints', 220);
hold(triplePendulumMotionAxes, 'off');
set(triplePendulumMotionAxes, 'XLimMode', 'manual', 'YLimMode', 'manual');

triplePendulumAngleAxes = subplot(2, 3, 3, ...
  'Parent', triplePendulumFigure);
hold(triplePendulumAngleAxes, 'on');
triplePendulumAngleTraces = gobjects(1, 3);
for rodIndex = 1:3
  triplePendulumAngleTraces(rodIndex) = animatedline(...
    triplePendulumAngleAxes, 'Color', triplePendulumColors(rodIndex, :), ...
    'LineWidth', 1.5);
end
hold(triplePendulumAngleAxes, 'off');
xlim(triplePendulumAngleAxes, [0 triplePendulumDuration]);
ylim(triplePendulumAngleAxes, [-190 190]);
grid(triplePendulumAngleAxes, 'on');
xlabel(triplePendulumAngleAxes, 'Time (s)');
ylabel(triplePendulumAngleAxes, 'Wrapped angle (deg)');
title(triplePendulumAngleAxes, 'Angular motion');
legend(triplePendulumAngleAxes, {'rod 1', 'rod 2', 'rod 3'}, ...
  'Location', 'best');

triplePendulumEnergyAxes = subplot(2, 3, 6, ...
  'Parent', triplePendulumFigure);
triplePendulumEnergyTrace = animatedline(triplePendulumEnergyAxes, ...
  'Color', [0.08 0.52 0.36], 'LineWidth', 1.7);
xlim(triplePendulumEnergyAxes, [0 triplePendulumDuration]);
triplePendulumEnergyLimit = max(1e-3, ...
  1.2e6 * max(abs(triplePendulumEnergyDrift)));
ylim(triplePendulumEnergyAxes, ...
  [-triplePendulumEnergyLimit triplePendulumEnergyLimit]);
grid(triplePendulumEnergyAxes, 'on');
xlabel(triplePendulumEnergyAxes, 'Time (s)');
ylabel(triplePendulumEnergyAxes, 'Relative drift (ppm)');
title(triplePendulumEnergyAxes, 'Mechanical energy conservation');

% Interactive playback controls (live speed, pause/resume, reset). They sit in
% the figure bottom margin and are polled by the animation loop.
uicontrol('Parent', triplePendulumFigure, 'Style', 'text', ...
  'String', 'Playback speed', 'Position', [40 46 120 18], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.97 0.98 1.00], ...
  'ForegroundColor', [0.16 0.20 0.28]);
triplePendulumSpeedSlider = uicontrol('Parent', triplePendulumFigure, ...
  'Style', 'slider', 'Min', 0.25, 'Max', 2.0, ...
  'Value', triplePendulumPlaybackSpeed, 'Position', [40 22 250 20]);
triplePendulumSpeedText = uicontrol('Parent', triplePendulumFigure, ...
  'Style', 'text', 'String', sprintf('%.2fx', triplePendulumPlaybackSpeed), ...
  'Position', [298 22 60 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.97 0.98 1.00], 'ForegroundColor', [0.16 0.20 0.28]);
triplePendulumPauseButton = uicontrol('Parent', triplePendulumFigure, ...
  'Style', 'pushbutton', 'String', 'Pause', 'Position', [380 18 100 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', ~get(source, 'UserData')));
triplePendulumResetButton = uicontrol('Parent', triplePendulumFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', 'Position', [490 18 100 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

% Lift the motion axes so its x-label clears the control strip below it.
triplePendulumMotionPosition = get(triplePendulumMotionAxes, 'Position');
triplePendulumMotionPosition(2) = triplePendulumMotionPosition(2) + 0.06;
triplePendulumMotionPosition(4) = triplePendulumMotionPosition(4) - 0.06;
set(triplePendulumMotionAxes, 'Position', triplePendulumMotionPosition);

if triplePendulumAnimationEnabled
  triplePendulumAnimationState = struct();
  triplePendulumAnimationState.figure = triplePendulumFigure;
  triplePendulumAnimationState.motionAxes = triplePendulumMotionAxes;
  triplePendulumAnimationState.rodPatches = triplePendulumRodPatches;
  triplePendulumAnimationState.jointMarkers = triplePendulumJointMarkers;
  triplePendulumAnimationState.centerMarkers = triplePendulumCenterMarkers;
  triplePendulumAnimationState.tipTrail = triplePendulumTipTrail;
  triplePendulumAnimationState.angleTraces = triplePendulumAngleTraces;
  triplePendulumAnimationState.energyTrace = triplePendulumEnergyTrace;
  triplePendulumAnimationState.time = triplePendulumTime;
  triplePendulumAnimationState.angles = triplePendulumAngles;
  triplePendulumAnimationState.energyDrift = triplePendulumEnergyDrift;
  triplePendulumAnimationState.lengths = triplePendulumLengths;
  triplePendulumAnimationState.centers = triplePendulumCenters;
  triplePendulumAnimationState.frameCount = ...
    max(2, triplePendulumAnimationFrames);
  % Simulated seconds represented by one animation frame; combined with the
  % live playback speed this paces every frame to real time.
  triplePendulumFrameSimDelta = ...
    triplePendulumTime(end) / triplePendulumAnimationState.frameCount;
  while isgraphics(triplePendulumFigure)
    triplePendulumFrameIndex = 1;
    triplePendulumFrameClock = tic();
    while triplePendulumFrameIndex <= ...
      triplePendulumAnimationState.frameCount
      if ~isgraphics(triplePendulumFigure)
        break
      end
      % Reset restarts the current replay from the first frame.
      if isgraphics(triplePendulumResetButton) && ...
        triplePendulumResetButton.UserData
        set(triplePendulumResetButton, 'UserData', false);
        if ~triplePendulumClearAnimation(triplePendulumAnimationState)
          break
        end
        triplePendulumFrameIndex = 1;
        triplePendulumFrameClock = tic();
        continue
      end
      % Pause holds the current frame while the toggle button is on.
      if isgraphics(triplePendulumPauseButton) && ...
        triplePendulumPauseButton.UserData
        set(triplePendulumPauseButton, 'String', 'Resume');
        drawnow();
        pause(0.05);
        triplePendulumFrameClock = tic();
        continue
      end
      if isgraphics(triplePendulumPauseButton)
        set(triplePendulumPauseButton, 'String', 'Pause');
      end
      if ~triplePendulumAnimationFrame(...
        triplePendulumAnimationState, triplePendulumFrameIndex)
        break
      end
      drawnow();
      if triplePendulumRealTime
        % Pace this frame to its wall-clock instant so the pendulum swings at
        % true physical speed; a slower or faster speed just scales the wait.
        triplePendulumSpeed = triplePendulumPlaybackSpeed;
        if isgraphics(triplePendulumSpeedSlider)
          triplePendulumSpeed = max(triplePendulumSpeedSlider.Value, 0.05);
          if isgraphics(triplePendulumSpeedText)
            set(triplePendulumSpeedText, 'String', ...
              sprintf('%.2fx', triplePendulumSpeed));
          end
        end
        triplePendulumRemaining = triplePendulumFrameSimDelta / ...
          triplePendulumSpeed - toc(triplePendulumFrameClock);
        if triplePendulumRemaining > 0
          pause(triplePendulumRemaining);
        end
      elseif triplePendulumFramePause > 0
        pause(triplePendulumFramePause);
      end
      triplePendulumFrameClock = tic();
      triplePendulumFrameIndex = triplePendulumFrameIndex + 1;
    end
    if ~triplePendulumAnimationContinuous || ...
      ~ isgraphics(triplePendulumFigure)
      break
    end
    if ~triplePendulumClearAnimation(triplePendulumAnimationState)
      break
    end
  end
end
%=============================================================================
function coupling = triplePendulumCouplingMatrix(lengths, centers, masses)
  bodyCount = numel(lengths);
  coupling = zeros(bodyCount);
  for rowIndex = 1:bodyCount
    for columnIndex = 1:bodyCount
      firstBody = max(rowIndex, columnIndex);
      for bodyIndex = firstBody:bodyCount
        rowArm = lengths(rowIndex);
        columnArm = lengths(columnIndex);
        if bodyIndex == rowIndex
          rowArm = centers(bodyIndex);
        end
        if bodyIndex == columnIndex
          columnArm = centers(bodyIndex);
        end
        coupling(rowIndex, columnIndex) = ...
          coupling(rowIndex, columnIndex) + ...
          masses(bodyIndex) * rowArm * columnArm;
      end
    end
  end
end
%=============================================================================
function moments = triplePendulumGravityMoments(lengths, centers, masses)
  bodyCount = numel(lengths);
  moments = zeros(bodyCount, 1);
  for rodIndex = 1:bodyCount
    moments(rodIndex) = masses(rodIndex) * centers(rodIndex);
    if rodIndex < bodyCount
      moments(rodIndex) = moments(rodIndex) + ...
        lengths(rodIndex) * sum(masses(rodIndex + 1:end));
    end
  end
end
%=============================================================================
function derivative = triplePendulumRhs(~, state, gravity, coupling, ...
  gravityLever, inertias)
  bodyCount = numel(gravityLever);
  angles = state(1:bodyCount);
  angularRates = state(bodyCount + 1:2 * bodyCount);
  massMatrix = triplePendulumMassMatrix(angles, coupling, inertias);
  angleDifference = repmat(angles, 1, bodyCount) - ...
    repmat(angles.', bodyCount, 1);
  centrifugal = (coupling .* sin(angleDifference)) * angularRates .^ 2;
  gravityTorque = gravity * gravityLever .* sin(angles);
  angularAccelerations = -massMatrix \ (centrifugal + gravityTorque);
  derivative = [angularRates; angularAccelerations];
end
%=============================================================================
function massMatrix = triplePendulumMassMatrix(angles, coupling, inertias)
  bodyCount = numel(angles);
  angleDifference = repmat(angles, 1, bodyCount) - ...
    repmat(angles.', bodyCount, 1);
  massMatrix = coupling .* cos(angleDifference) + diag(inertias);
end
%=============================================================================
function energy = triplePendulumTotalEnergy(angles, angularRates, ...
  gravity, coupling, gravityLever, inertias)
  sampleCount = size(angles, 1);
  bodyCount = size(angles, 2);
  kinetic = 0.5 * sum(angularRates .^ 2 .* ...
    repmat(inertias.', sampleCount, 1), 2);
  for rowIndex = 1:bodyCount
    for columnIndex = 1:bodyCount
      kinetic = kinetic + 0.5 * coupling(rowIndex, columnIndex) * ...
        cos(angles(:, rowIndex) - angles(:, columnIndex)) .* ...
        angularRates(:, rowIndex) .* angularRates(:, columnIndex);
    end
  end
  potential = -gravity * sum(cos(angles) .* ...
    repmat(gravityLever.', sampleCount, 1), 2);
  energy = kinetic + potential;
end
%=============================================================================
function [joints, centersOfMass] = triplePendulumGeometry(...
  angles, lengths, centers)
  bodyCount = numel(lengths);
  joints = zeros(bodyCount + 1, 2);
  centersOfMass = zeros(bodyCount, 2);
  for rodIndex = 1:bodyCount
    direction = [sin(angles(rodIndex)) -cos(angles(rodIndex))];
    centersOfMass(rodIndex, :) = ...
      joints(rodIndex, :) + centers(rodIndex) * direction;
    joints(rodIndex + 1, :) = ...
      joints(rodIndex, :) + lengths(rodIndex) * direction;
  end
end
%=============================================================================
function polygon = triplePendulumRodPolygon(firstJoint, secondJoint, width)
  direction = secondJoint - firstJoint;
  direction = direction / hypot(direction(1), direction(2));
  normal = 0.5 * width * [-direction(2) direction(1)];
  polygon = [firstJoint + normal; secondJoint + normal; ...
    secondJoint - normal; firstJoint - normal];
end
%=============================================================================
function keepAnimating = triplePendulumAnimationFrame(animationState, frameIndex)
  keepAnimating = triplePendulumAnimationHandlesValid(animationState);
  if ~keepAnimating
    return
  end
  sampleCount = numel(animationState.time);
  sampleIndex = 1 + round((sampleCount - 1) * (frameIndex - 1) / ...
    (animationState.frameCount - 1));
  angles = animationState.angles(sampleIndex, :).';
  [joints, centersOfMass] = triplePendulumGeometry(angles, ...
    animationState.lengths, animationState.centers);
  for rodIndex = 1:3
    polygon = triplePendulumRodPolygon(joints(rodIndex, :), ...
      joints(rodIndex + 1, :), 0.075);
    set(animationState.rodPatches(rodIndex), ...
      'XData', polygon(:, 1), 'YData', polygon(:, 2));
    addpoints(animationState.angleTraces(rodIndex), ...
      animationState.time(sampleIndex), ...
      atan2(sin(angles(rodIndex)), cos(angles(rodIndex))) * 180 / pi);
  end
  set(animationState.jointMarkers, ...
    'XData', joints(:, 1), 'YData', joints(:, 2));
  set(animationState.centerMarkers, ...
    'XData', centersOfMass(:, 1), 'YData', centersOfMass(:, 2));
  addpoints(animationState.tipTrail, joints(end, 1), joints(end, 2));
  addpoints(animationState.energyTrace, ...
    animationState.time(sampleIndex), ...
    1e6 * animationState.energyDrift(sampleIndex));
  title(animationState.motionAxes, ...
    sprintf('Three coupled rigid rods — t = %.2f s', ...
    animationState.time(sampleIndex)));
  keepAnimating = triplePendulumAnimationHandlesValid(animationState);
end
%=============================================================================
function valid = triplePendulumAnimationHandlesValid(animationState)
  valid = isgraphics(animationState.figure) && ...
    isgraphics(animationState.motionAxes) && ...
    all(isgraphics(animationState.rodPatches)) && ...
    isgraphics(animationState.jointMarkers) && ...
    isgraphics(animationState.centerMarkers) && ...
    isgraphics(animationState.tipTrail) && ...
    all(isgraphics(animationState.angleTraces)) && ...
    isgraphics(animationState.energyTrace);
end
%=============================================================================
function cleared = triplePendulumClearAnimation(animationState)
  cleared = triplePendulumAnimationHandlesValid(animationState);
  if ~cleared
    return
  end
  clearpoints(animationState.tipTrail);
  clearpoints(animationState.energyTrace);
  for rodIndex = 1:3
    clearpoints(animationState.angleTraces(rodIndex));
  end
end
%=============================================================================
