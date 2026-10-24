%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compare low-drag and high-downforce race-car setups over one animated lap.
if ~exist('raceFigureVisible', 'var')
  raceFigureVisible = 'on';
end
if ~exist('raceAnimationEnabled', 'var')
  raceAnimationEnabled = true;
end
if ~exist('raceAnimationFrames', 'var')
  raceAnimationFrames = 240;
end
if ~exist('raceAnimationContinuous', 'var')
  raceAnimationContinuous = strcmp(raceFigureVisible, 'on');
end
if ~exist('raceReuseRequested', 'var')
  raceReuseRequested = false;
end
raceCircuitNames = {'Albert Park'; 'Monza'; 'Monaco'; ...
  'Spa-Francorchamps'; 'Suzuka'};
if ~exist('raceCircuitName', 'var')
  raceCircuitName = raceCircuitNames{1};
end
selectedCircuitIndex = find(strcmp(raceCircuitNames, raceCircuitName), 1);
if isempty(selectedCircuitIndex)
  selectedCircuitIndex = 1;
  raceCircuitName = raceCircuitNames{selectedCircuitIndex};
end

pointCount = 720;
[trackX, trackY, targetTrackLength] = ...
  formulaRacingCircuit(raceCircuitName, pointCount);

nextPoint = [2:pointCount 1];
previousPoint = [pointCount 1:pointCount - 1];
segmentLength = hypot(trackX(nextPoint) - trackX, ...
  trackY(nextPoint) - trackY);
trackScale = targetTrackLength / sum(segmentLength);
trackX = trackScale * trackX;
trackY = trackScale * trackY;
segmentLength = trackScale * segmentLength;

thetaStep = 2 * pi / pointCount;
firstX = (trackX(nextPoint) - trackX(previousPoint)) / (2 * thetaStep);
firstY = (trackY(nextPoint) - trackY(previousPoint)) / (2 * thetaStep);
secondX = (trackX(nextPoint) - 2 * trackX + ...
  trackX(previousPoint)) / thetaStep ^ 2;
secondY = (trackY(nextPoint) - 2 * trackY + ...
  trackY(previousPoint)) / thetaStep ^ 2;
trackCurvature = abs(firstX .* secondY - firstY .* secondX) ./ ...
  max((firstX .^ 2 + firstY .^ 2) .^ 1.5, eps);

vehicleMass = 798;
gravity = 9.81;
airDensity = 1.225;
enginePower = 750000;
rollingCoefficient = 0.015;
rollingForce = rollingCoefficient * vehicleMass * gravity;
tireFriction = 1.65;
dragArea = [1.25 2.45];
liftArea = [2.20 10.00];
driveFriction = [1.35 1.70];
brakeFriction = [1.70 2.15];
speedProfiles = zeros(2, pointCount);
terminalSpeeds = zeros(1, 2);

for setupIndex = 1:2
  terminalSpeed = 90;
  for iteration = 1:25
    powerBalance = 0.5 * airDensity * dragArea(setupIndex) * ...
      terminalSpeed ^ 3 + rollingForce * terminalSpeed - enginePower;
    powerSlope = 1.5 * airDensity * dragArea(setupIndex) * ...
      terminalSpeed ^ 2 + rollingForce;
    terminalSpeed = max(10, terminalSpeed - powerBalance / powerSlope);
  end
  terminalSpeeds(setupIndex) = terminalSpeed;

  lateralDenominator = vehicleMass * trackCurvature - ...
    0.5 * tireFriction * airDensity * liftArea(setupIndex);
  cornerSpeed = sqrt(tireFriction * vehicleMass * gravity ./ ...
    max(lateralDenominator, eps));
  velocity = min(terminalSpeed, cornerSpeed);

  for iteration = 1:35
    for trackIndex = 1:pointCount
      followingIndex = nextPoint(trackIndex);
      currentSpeed = velocity(trackIndex);
      downforce = 0.5 * airDensity * liftArea(setupIndex) * currentSpeed ^ 2;
      gripAcceleration = driveFriction(setupIndex) * ...
        (gravity + downforce / vehicleMass);
      powerAcceleration = enginePower / ...
        (vehicleMass * max(currentSpeed, 15));
      dragAcceleration = (0.5 * airDensity * dragArea(setupIndex) * ...
        currentSpeed ^ 2 + rollingForce) / vehicleMass;
      acceleration = max(0, min(gripAcceleration, powerAcceleration) - ...
        dragAcceleration);
      reachableSpeed = sqrt(currentSpeed ^ 2 + ...
        2 * acceleration * segmentLength(trackIndex));
      velocity(followingIndex) = min(velocity(followingIndex), reachableSpeed);
    end

    for trackIndex = pointCount:-1:1
      followingIndex = nextPoint(trackIndex);
      followingSpeed = velocity(followingIndex);
      downforce = 0.5 * airDensity * liftArea(setupIndex) * followingSpeed ^ 2;
      dragAcceleration = (0.5 * airDensity * dragArea(setupIndex) * ...
        followingSpeed ^ 2 + rollingForce) / vehicleMass;
      brakingAcceleration = brakeFriction(setupIndex) * ...
        (gravity + downforce / vehicleMass) + dragAcceleration;
      permittedSpeed = sqrt(followingSpeed ^ 2 + ...
        2 * brakingAcceleration * segmentLength(trackIndex));
      velocity(trackIndex) = min(velocity(trackIndex), permittedSpeed);
    end
  end
  speedProfiles(setupIndex, :) = velocity;
end

distanceMeters = [0 cumsum(segmentLength)];
distanceKilometers = distanceMeters / 1000;
closedTrackX = [trackX trackX(1)];
closedTrackY = [trackY trackY(1)];
closedSpeeds = [speedProfiles speedProfiles(:, 1)];
segmentTimes = 2 * segmentLength ./ ...
  (speedProfiles + speedProfiles(:, nextPoint));
cumulativeTimes = [zeros(2, 1) cumsum(segmentTimes, 2)];
lapTimes = cumulativeTimes(:, end);
lapTimeDelta = cumulativeTimes(1, :) - cumulativeTimes(2, :);

lowDragColor = [0.10 0.42 0.86];
highDownforceColor = [0.88 0.18 0.32];
reuseFigure = false;
if exist('raceFigureHandle', 'var') && ~isempty(raceFigureHandle)
  reuseFigure = isgraphics(raceFigureHandle);
end
if raceReuseRequested && ~reuseFigure
  raceReuseRequested = false;
  return
end
if reuseFigure
  figureHandle = raceFigureHandle;
  try
    clf(figureHandle);
    set(figureHandle, 'Name', 'Formula racing aerodynamic trade-off', ...
      'NumberTitle', 'off', 'Color', 'w', 'Visible', raceFigureVisible);
  catch exception
    raceReuseRequested = false;
    if contains(exception.message, 'Invalid graphics object')
      return
    end
    rethrow(exception);
  end
else
  figureHandle = figure('Name', 'Formula racing aerodynamic trade-off', ...
    'NumberTitle', 'off', 'Color', 'w', 'Visible', raceFigureVisible, ...
    'Position', [80 80 1220 760]);
end
raceFigureHandle = figureHandle;
raceReuseRequested = false;
uicontrol('Parent', figureHandle, 'Style', 'text', 'String', 'Circuit', ...
  'Position', [20 724 55 24], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', 'w');
circuitSelector = uicontrol('Parent', figureHandle, 'Style', 'popupmenu', ...
  'String', raceCircuitNames, 'Value', selectedCircuitIndex, ...
  'Position', [78 722 190 28], 'Tag', 'formulaRacingCircuitSelector');

trackAxes = subplot(2, 2, 1, 'Parent', figureHandle);
plot(trackAxes, closedTrackX, closedTrackY, 'Color', [0.78 0.80 0.83], ...
  'LineWidth', 5);
hold(trackAxes, 'on');
lowDragTrail = animatedline(trackAxes, 'Color', lowDragColor, 'LineWidth', 2);
highDownforceTrail = animatedline(trackAxes, 'Color', highDownforceColor, ...
  'LineWidth', 2);
lowDragCar = plot(trackAxes, trackX(1), trackY(1), 'o', ...
  'Color', lowDragColor, 'MarkerFaceColor', lowDragColor, 'MarkerSize', 8);
highDownforceCar = plot(trackAxes, trackX(1), trackY(1), 'o', ...
  'Color', highDownforceColor, 'MarkerFaceColor', highDownforceColor, ...
  'MarkerSize', 8);
plot(trackAxes, trackX(1), trackY(1), 'ks', 'MarkerFaceColor', 'w', ...
  'MarkerSize', 7);
hold(trackAxes, 'off');
axis(trackAxes, 'equal');
axis(trackAxes, 'off');
title(trackAxes, [raceCircuitName, ' - circuit position']);

speedAxes = subplot(2, 2, 2, 'Parent', figureHandle);
hold(speedAxes, 'on');
lowDragSpeedTrace = animatedline(speedAxes, 'Color', lowDragColor, ...
  'LineWidth', 1.7);
highDownforceSpeedTrace = animatedline(speedAxes, ...
  'Color', highDownforceColor, 'LineWidth', 1.7);
speedCursorLow = plot(speedAxes, [0 0], [0 380], '--', 'Color', lowDragColor);
speedCursorHigh = plot(speedAxes, [0 0], [0 380], '--', ...
  'Color', highDownforceColor);
hold(speedAxes, 'off');
axis(speedAxes, [0 distanceKilometers(end) 0 380]);
grid(speedAxes, 'on');
xlabel(speedAxes, 'Distance (km)');
ylabel(speedAxes, 'Speed (km/h)');
title(speedAxes, 'Speed profile comparison');
legend(speedAxes, 'Low drag', 'High downforce', 'Location', 'best');

deltaAxes = subplot(2, 2, [3 4], 'Parent', figureHandle);
hold(deltaAxes, 'on');
deltaTrace = animatedline(deltaAxes, 'Color', [0.20 0.22 0.28], ...
  'LineWidth', 1.8);
plot(deltaAxes, [0 distanceKilometers(end)], [0 0], 'Color', [0.55 0.55 0.58]);
deltaMargin = max(0.2, 0.08 * (max(lapTimeDelta) - min(lapTimeDelta)));
deltaLimits = [min(lapTimeDelta) - deltaMargin ...
  max(lapTimeDelta) + deltaMargin];
deltaCursor = plot(deltaAxes, [0 0], ...
  deltaLimits, '--', 'Color', highDownforceColor);
hold(deltaAxes, 'off');
axis(deltaAxes, [0 distanceKilometers(end) deltaLimits]);
grid(deltaAxes, 'on');
xlabel(deltaAxes, 'Distance (km)');
ylabel(deltaAxes, 'Low drag - high downforce (s)');
title(deltaAxes, sprintf('Lap-time delta: %.2f s versus %.2f s', ...
  lapTimes(1), lapTimes(2)));

if raceAnimationEnabled
  animationState = struct();
  animationState.figure = figureHandle;
  animationState.lowDragCar = lowDragCar;
  animationState.highDownforceCar = highDownforceCar;
  animationState.lowDragTrail = lowDragTrail;
  animationState.highDownforceTrail = highDownforceTrail;
  animationState.speedCursorLow = speedCursorLow;
  animationState.speedCursorHigh = speedCursorHigh;
  animationState.deltaCursor = deltaCursor;
  animationState.lowDragSpeedTrace = lowDragSpeedTrace;
  animationState.highDownforceSpeedTrace = highDownforceSpeedTrace;
  animationState.deltaTrace = deltaTrace;
  animationState.trackAxes = trackAxes;
  animationState.distanceMeters = distanceMeters;
  animationState.closedTrackX = closedTrackX;
  animationState.closedTrackY = closedTrackY;
  animationState.closedSpeeds = closedSpeeds;
  animationState.cumulativeTimes = cumulativeTimes;
  animationState.lapTimes = lapTimes;
  animationState.lapTimeDelta = lapTimeDelta;
  animationState.circuitName = raceCircuitName;
  animationState.frameCount = max(2, raceAnimationFrames);
  restartRequested = false;
  while isgraphics(figureHandle)
    for frameIndex = 1:animationState.frameCount
      if ~formulaRacingAnimationFrame(animationState, frameIndex)
        break
      end
      drawnow();
      pause(0.015);
      if isgraphics(circuitSelector) && ...
        circuitSelector.Value ~= selectedCircuitIndex
        raceCircuitName = ...
          raceCircuitNames{circuitSelector.Value};
        restartRequested = true;
        break
      end
    end
    if restartRequested || ~raceAnimationContinuous
      break
    end
    if ~isgraphics(figureHandle)
      break
    end
    if ~formulaRacingClearAnimation(animationState)
      break
    end
  end
  if restartRequested
    if isgraphics(figureHandle)
      raceReuseRequested = true;
      run([modulepath('graphics', 'root'), ...
        '/examples/formula_racing_aero_tradeoff.m']);
    end
    return
  end
end
%=============================================================================
function [trackX, trackY, targetTrackLength] = ...
  formulaRacingCircuit(circuitName, pointCount)
  theta = linspace(0, 2 * pi, pointCount + 1);
  theta(end) = [];
  switch circuitName
    case 'Monza'
      targetTrackLength = 5800;
      radialShape = 1 + 0.12 * sin(2 * theta + 0.4) + ...
        0.08 * cos(4 * theta - 0.3);
      trackX = radialShape .* ...
        (980 * cos(theta) + 170 * cos(2 * theta));
      trackY = radialShape .* ...
        (270 * sin(theta) - 55 * sin(3 * theta));
    case 'Monaco'
      targetTrackLength = 3300;
      radialShape = 1 + 0.30 * sin(3 * theta - 0.2) - ...
        0.17 * cos(4 * theta) + 0.10 * sin(6 * theta + 0.5);
      trackX = radialShape .* ...
        (520 * cos(theta) + 145 * sin(2 * theta));
      trackY = radialShape .* ...
        (350 * sin(theta) + 80 * cos(3 * theta));
    case 'Spa-Francorchamps'
      targetTrackLength = 7000;
      radialShape = 1 + 0.22 * sin(2 * theta - 0.4) + ...
        0.16 * cos(3 * theta + 0.5) + 0.07 * sin(5 * theta);
      trackX = radialShape .* ...
        (900 * cos(theta) + 125 * cos(2 * theta));
      trackY = radialShape .* ...
        (430 * sin(theta) - 90 * sin(2 * theta));
    case 'Suzuka'
      targetTrackLength = 5800;
      trackX = 800 * sin(theta) + 80 * sin(3 * theta);
      trackY = 360 * sin(2 * theta) + 45 * cos(3 * theta);
    otherwise
      targetTrackLength = 5278;
      radialShape = 1 + 0.34 * sin(3 * theta + 0.4) + ...
        0.21 * cos(5 * theta - 0.7) + 0.10 * sin(7 * theta);
      trackX = radialShape .* ...
        (760 * cos(theta) + 120 * cos(2 * theta));
      trackY = radialShape .* ...
        (340 * sin(theta) - 85 * sin(2 * theta));
  end
end
%=============================================================================
function keepAnimating = formulaRacingAnimationFrame(animationState, frameIndex)
  keepAnimating = formulaRacingAnimationHandlesValid(animationState);
  if ~keepAnimating
    return
  end
  animationDuration = max(animationState.lapTimes);
  animationTime = animationDuration * (frameIndex - 1) / ...
    (animationState.frameCount - 1);
  lowDragDistance = interp1(animationState.cumulativeTimes(1, :), ...
    animationState.distanceMeters, ...
    min(animationTime, animationState.lapTimes(1)), 'linear');
  highDownforceDistance = interp1(animationState.cumulativeTimes(2, :), ...
    animationState.distanceMeters, ...
    min(animationTime, animationState.lapTimes(2)), 'linear');
  lowDragX = interp1(animationState.distanceMeters, ...
    animationState.closedTrackX, lowDragDistance, 'linear');
  lowDragY = interp1(animationState.distanceMeters, ...
    animationState.closedTrackY, lowDragDistance, 'linear');
  highDownforceX = interp1(animationState.distanceMeters, ...
    animationState.closedTrackX, highDownforceDistance, 'linear');
  highDownforceY = interp1(animationState.distanceMeters, ...
    animationState.closedTrackY, highDownforceDistance, 'linear');
  try
    set(animationState.lowDragCar, 'XData', lowDragX, 'YData', lowDragY);
    set(animationState.highDownforceCar, ...
      'XData', highDownforceX, 'YData', highDownforceY);
    addpoints(animationState.lowDragTrail, lowDragX, lowDragY);
    addpoints(animationState.highDownforceTrail, ...
      highDownforceX, highDownforceY);
    set(animationState.speedCursorLow, ...
      'XData', [lowDragDistance lowDragDistance] / 1000);
    set(animationState.speedCursorHigh, ...
      'XData', [highDownforceDistance highDownforceDistance] / 1000);
    set(animationState.deltaCursor, ...
      'XData', [highDownforceDistance highDownforceDistance] / 1000);
    lowDragSpeed = interp1(animationState.distanceMeters, ...
      animationState.closedSpeeds(1, :), lowDragDistance, 'linear') * 3.6;
    highDownforceSpeed = interp1(animationState.distanceMeters, ...
      animationState.closedSpeeds(2, :), ...
      highDownforceDistance, 'linear') * 3.6;
    currentDelta = interp1(animationState.distanceMeters, ...
      animationState.lapTimeDelta, highDownforceDistance, 'linear');
    addpoints(animationState.lowDragSpeedTrace, ...
      lowDragDistance / 1000, lowDragSpeed);
    addpoints(animationState.highDownforceSpeedTrace, ...
      highDownforceDistance / 1000, highDownforceSpeed);
    addpoints(animationState.deltaTrace, ...
      highDownforceDistance / 1000, currentDelta);
    title(animationState.trackAxes, ...
      sprintf('%s - elapsed time: %.1f s', ...
      animationState.circuitName, animationTime));
  catch exception
    keepAnimating = false;
    if formulaRacingAnimationHandlesValid(animationState) && ...
      ~ contains(exception.message, 'Invalid graphics object')
      rethrow(exception);
    end
  end
end
%=============================================================================
function valid = formulaRacingAnimationHandlesValid(animationState)
  valid = isgraphics(animationState.figure) && ...
    isgraphics(animationState.lowDragCar) && ...
    isgraphics(animationState.highDownforceCar) && ...
    isgraphics(animationState.lowDragTrail) && ...
    isgraphics(animationState.highDownforceTrail) && ...
    isgraphics(animationState.speedCursorLow) && ...
    isgraphics(animationState.speedCursorHigh) && ...
    isgraphics(animationState.deltaCursor) && ...
    isgraphics(animationState.lowDragSpeedTrace) && ...
    isgraphics(animationState.highDownforceSpeedTrace) && ...
    isgraphics(animationState.deltaTrace) && ...
    isgraphics(animationState.trackAxes);
end
%=============================================================================
function cleared = formulaRacingClearAnimation(animationState)
  cleared = formulaRacingAnimationHandlesValid(animationState);
  if ~cleared
    return
  end
  try
    clearpoints(animationState.lowDragTrail);
    clearpoints(animationState.highDownforceTrail);
    clearpoints(animationState.lowDragSpeedTrace);
    clearpoints(animationState.highDownforceSpeedTrace);
    clearpoints(animationState.deltaTrace);
  catch exception
    cleared = false;
    if formulaRacingAnimationHandlesValid(animationState) && ...
      ~ contains(exception.message, 'Invalid graphics object')
      rethrow(exception);
    end
  end
end
%=============================================================================
