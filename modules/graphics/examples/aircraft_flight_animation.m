%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a fixed-wing aircraft with synchronized flight instruments.
if ~exist('flightFigureVisible', 'var')
  flightFigureVisible = 'on';
end
if ~exist('flightAnimationEnabled', 'var')
  flightAnimationEnabled = true;
end
if ~exist('flightAnimationFrames', 'var')
  flightAnimationFrames = 320;
end
if ~exist('flightAnimationContinuous', 'var')
  flightAnimationContinuous = strcmp(flightFigureVisible, 'on');
end

flightSampleCount = 720;
[flightTime, flightEast, flightNorth, flightAltitude, flightSpeed, ...
  flightRoll, flightPitch, flightYaw] = flightTelemetry(flightSampleCount);
[flightAircraftVertices, flightAircraftFaces] = flightAircraftGeometry();

figureHandle = figure('Name', 'Aircraft flight animation', ...
  'NumberTitle', 'off', 'Color', 'w', 'Visible', flightFigureVisible, ...
  'Position', [70 70 1260 760]);

flightAxes = subplot(2, 3, [1 2 4 5], 'Parent', figureHandle);
plot3(flightAxes, flightEast, flightNorth, flightAltitude, ...
  'Color', [0.78 0.81 0.86], 'LineWidth', 1.4);
hold(flightAxes, 'on');
flightTrail = animatedline(flightAxes, 'Color', [0.08 0.42 0.82], ...
  'LineWidth', 2.4);
flightAircraftVerticesInitial = flightAircraftTransform(...
  flightAircraftVertices, [flightEast(1) flightNorth(1) flightAltitude(1)], ...
  flightRoll(1), flightPitch(1), flightYaw(1));
aircraftPatch = patch('Parent', flightAxes, ...
  'Vertices', flightAircraftVerticesInitial, 'Faces', flightAircraftFaces, ...
  'FaceColor', [0.92 0.18 0.12], 'EdgeColor', [0.22 0.08 0.05], ...
  'LineWidth', 0.7);
plot3(flightAxes, flightEast(1), flightNorth(1), flightAltitude(1), ...
  'o', 'Color', [0.12 0.12 0.14], 'MarkerFaceColor', 'w', 'MarkerSize', 6);
hold(flightAxes, 'off');
eastMargin = 0.08 * (max(flightEast) - min(flightEast));
northMargin = 0.08 * (max(flightNorth) - min(flightNorth));
altitudeMargin = 0.16 * (max(flightAltitude) - min(flightAltitude));
axis(flightAxes, [min(flightEast) - eastMargin max(flightEast) + eastMargin ...
  min(flightNorth) - northMargin max(flightNorth) + northMargin ...
  max(0, min(flightAltitude) - altitudeMargin) ...
  max(flightAltitude) + altitudeMargin]);
axis(flightAxes, 'vis3d');
grid(flightAxes, 'on');
view(flightAxes, 38, 27);
xlabel(flightAxes, 'East (m)');
ylabel(flightAxes, 'North (m)');
zlabel(flightAxes, 'Altitude (m)');
title(flightAxes, 'Synthetic flight path');

telemetryAxes = subplot(2, 3, 3, 'Parent', figureHandle);
yyaxis(telemetryAxes, 'left');
altitudeTrace = animatedline(telemetryAxes, ...
  'Color', [0.10 0.42 0.86], 'LineWidth', 1.8);
ylim(telemetryAxes, [floor(min(flightAltitude) / 100) * 100 ...
  ceil(max(flightAltitude) / 100) * 100]);
ylabel(telemetryAxes, 'Altitude (m)');
yyaxis(telemetryAxes, 'right');
speedTrace = animatedline(telemetryAxes, ...
  'Color', [0.90 0.28 0.12], 'LineWidth', 1.8);
speedMargin = 0.12 * (max(flightSpeed) - min(flightSpeed));
ylim(telemetryAxes, [min(flightSpeed) - speedMargin ...
  max(flightSpeed) + speedMargin]);
ylabel(telemetryAxes, 'True airspeed (m/s)');
xlim(telemetryAxes, [flightTime(1) flightTime(end)]);
grid(telemetryAxes, 'on');
xlabel(telemetryAxes, 'Time (s)');
title(telemetryAxes, 'Flight telemetry');

horizonAxes = subplot(2, 3, 6, 'Parent', figureHandle);
hold(horizonAxes, 'on');
horizonCircleAngle = linspace(0, 2 * pi, 121);
horizonSky = patch('Parent', horizonAxes, ...
  'XData', cos(horizonCircleAngle), 'YData', sin(horizonCircleAngle), ...
  'ZData', -2 * ones(size(horizonCircleAngle)), ...
  'FaceColor', [0.30 0.62 0.88], 'EdgeColor', 'none');
horizonGroundVertices = flightHorizonGround(flightRoll(1), flightPitch(1));
horizonGround = patch('Parent', horizonAxes, ...
  'XData', horizonGroundVertices(:, 1), ...
  'YData', horizonGroundVertices(:, 2), ...
  'ZData', -ones(size(horizonGroundVertices, 1), 1), ...
  'FaceColor', [0.52 0.34 0.16], 'EdgeColor', 'none');
horizonLineVertices = flightHorizonLine(flightRoll(1), flightPitch(1));
horizonLine = plot3(horizonAxes, horizonLineVertices(:, 1), ...
  horizonLineVertices(:, 2), zeros(2, 1), 'w', 'LineWidth', 2);
plot3(horizonAxes, [-0.62 -0.18], [0 0], [0 0], ...
  'Color', [1.00 0.82 0.12], ...
  'LineWidth', 3);
plot3(horizonAxes, [0.18 0.62], [0 0], [0 0], ...
  'Color', [1.00 0.82 0.12], ...
  'LineWidth', 3);
plot3(horizonAxes, [0 0], [-0.10 0.10], [0 0], ...
  'Color', [1.00 0.82 0.12], ...
  'LineWidth', 3);
plot3(horizonAxes, [-0.75 0 0.75], [0.72 0.92 0.72], [0 0 0], 'w.', ...
  'MarkerSize', 12);
plot3(horizonAxes, cos(horizonCircleAngle), sin(horizonCircleAngle), ...
  zeros(size(horizonCircleAngle)), ...
  'Color', [0.16 0.18 0.22], 'LineWidth', 2.5);
horizonStatus = text(horizonAxes, 0, 0.78, 'Roll +0.0 deg   Pitch +0.0 deg', ...
  'Color', 'w', 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
hold(horizonAxes, 'off');
axis(horizonAxes, [-1 1 -1 1]);
axis(horizonAxes, 'equal');
axis(horizonAxes, 'off');
view(horizonAxes, 2);
title(horizonAxes, 'Artificial horizon');

if flightAnimationEnabled
  animationState = struct();
  animationState.figure = figureHandle;
  animationState.aircraft = aircraftPatch;
  animationState.flightAxes = flightAxes;
  animationState.telemetryAxes = telemetryAxes;
  animationState.horizonAxes = horizonAxes;
  animationState.trail = flightTrail;
  animationState.altitudeTrace = altitudeTrace;
  animationState.speedTrace = speedTrace;
  animationState.horizonGround = horizonGround;
  animationState.horizonLine = horizonLine;
  animationState.horizonStatus = horizonStatus;
  animationState.time = flightTime;
  animationState.east = flightEast;
  animationState.north = flightNorth;
  animationState.altitude = flightAltitude;
  animationState.speed = flightSpeed;
  animationState.roll = flightRoll;
  animationState.pitch = flightPitch;
  animationState.yaw = flightYaw;
  animationState.aircraftVertices = flightAircraftVertices;
  animationState.frameCount = max(2, flightAnimationFrames);
  while isgraphics(figureHandle)
    for frameIndex = 1:animationState.frameCount
      if ~flightAnimationFrame(animationState, frameIndex)
        break
      end
      drawnow();
      pause(0.015);
    end
    if ~flightAnimationContinuous || ~isgraphics(figureHandle)
      break
    end
    if ~flightClearAnimation(animationState)
      break
    end
  end
end
%=============================================================================
function [time, east, north, altitude, speed, roll, pitch, yaw] = ...
  flightTelemetry(sampleCount)
  time = linspace(0, 120, sampleCount);
  phase = 2 * pi * time / time(end);
  east = 1280 * cos(phase) + 210 * cos(2 * phase - 0.35);
  north = 860 * sin(phase) + 170 * sin(3 * phase + 0.25);
  altitude = 320 + 5.5 * time + 85 * sin(2 * phase - 0.6);
  timeStep = time(2) - time(1);
  eastRate = flightFiniteDifference(east, timeStep);
  northRate = flightFiniteDifference(north, timeStep);
  climbRate = flightFiniteDifference(altitude, timeStep);
  horizontalSpeed = hypot(eastRate, northRate);
  speed = sqrt(horizontalSpeed .^ 2 + climbRate .^ 2);
  yaw = atan2(northRate, eastRate);
  pitch = atan2(climbRate, horizontalSpeed);
  headingRate = flightFiniteDifference(unwrap(yaw), timeStep);
  roll = atan(horizontalSpeed .* headingRate / 9.81);
  rollLimit = 55 * pi / 180;
  roll = max(-rollLimit, min(rollLimit, roll));
end
%=============================================================================
function derivative = flightFiniteDifference(values, timeStep)
  derivative = zeros(size(values));
  derivative(1) = (values(2) - values(1)) / timeStep;
  derivative(end) = (values(end) - values(end - 1)) / timeStep;
  derivative(2:end - 1) = ...
    (values(3:end) - values(1:end - 2)) / (2 * timeStep);
end
%=============================================================================
function [vertices, faces] = flightAircraftGeometry()
  vertices = [
    55, 0, 0;
    4, 0, 4;
    4, 0, -4;
    -38, 0, 0;
    0, -52, 0;
    0, 52, 0;
    -29, -18, 2;
    -29, 18, 2;
    -28, 0, 18];
  faces = [
    1 5 2;
    1 2 6;
    1 3 5;
    1 6 3;
    1 2 3;
    4 3 2;
    4 7 2;
    4 2 8;
    4 3 7;
    4 8 3;
    4 2 9];
end
%=============================================================================
function transformed = flightAircraftTransform(vertices, position, ...
  roll, pitch, yaw)
  rollMatrix = [1 0 0; 0 cos(roll) -sin(roll); ...
    0 sin(roll) cos(roll)];
  pitchMatrix = [cos(pitch) 0 sin(pitch); 0 1 0; ...
    -sin(pitch) 0 cos(pitch)];
  yawMatrix = [cos(yaw) -sin(yaw) 0; ...
    sin(yaw) cos(yaw) 0; 0 0 1];
  rotation = yawMatrix * pitchMatrix * rollMatrix;
  transformed = vertices * rotation.' + ...
    repmat(position, size(vertices, 1), 1);
end
%=============================================================================
function vertices = flightHorizonGround(roll, pitch)
  circleAngle = linspace(0, 2 * pi, 121);
  circleAngle(end) = [];
  circle = [cos(circleAngle(:)) sin(circleAngle(:))];
  horizonAngle = -roll;
  horizonNormal = [-sin(horizonAngle); cos(horizonAngle)];
  horizonOffset = max(-0.95, min(0.95, -1.5 * pitch));
  vertices = flightClipCircle(circle, horizonNormal, horizonOffset);
end
%=============================================================================
function vertices = flightHorizonLine(roll, pitch)
  horizonAngle = -roll;
  direction = [cos(horizonAngle) sin(horizonAngle)];
  normal = [-sin(horizonAngle) cos(horizonAngle)];
  offset = max(-0.95, min(0.95, -1.5 * pitch));
  halfLength = sqrt(1 - offset ^ 2);
  center = offset * normal;
  vertices = [center - halfLength * direction; ...
    center + halfLength * direction];
end
%=============================================================================
function clipped = flightClipCircle(circle, normal, offset)
  clipped = zeros(0, 2);
  pointCount = size(circle, 1);
  for pointIndex = 1:pointCount
    nextIndex = mod(pointIndex, pointCount) + 1;
    currentPoint = circle(pointIndex, :);
    nextPoint = circle(nextIndex, :);
    currentValue = currentPoint * normal - offset;
    nextValue = nextPoint * normal - offset;
    currentInside = currentValue <= 0;
    nextInside = nextValue <= 0;
    if currentInside
      clipped(end + 1, :) = currentPoint;
    end
    if currentInside ~= nextInside
      fraction = currentValue / (currentValue - nextValue);
      intersection = currentPoint + fraction * (nextPoint - currentPoint);
      clipped(end + 1, :) = intersection;
    end
  end
end
%=============================================================================
function keepAnimating = flightAnimationFrame(animationState, frameIndex)
  keepAnimating = flightAnimationHandlesValid(animationState);
  if ~keepAnimating
    return
  end
  sampleCount = numel(animationState.time);
  sampleIndex = round(1 + (sampleCount - 1) * (frameIndex - 1) / ...
    (animationState.frameCount - 1));
  position = [animationState.east(sampleIndex) ...
    animationState.north(sampleIndex) ...
    animationState.altitude(sampleIndex)];
  try
    aircraftVertices = flightAircraftTransform(...
      animationState.aircraftVertices, position, ...
      animationState.roll(sampleIndex), ...
      animationState.pitch(sampleIndex), ...
      animationState.yaw(sampleIndex));
    set(animationState.aircraft, 'Vertices', aircraftVertices);
    addpoints(animationState.trail, position(1), position(2), position(3));
    addpoints(animationState.altitudeTrace, ...
      animationState.time(sampleIndex), position(3));
    addpoints(animationState.speedTrace, ...
      animationState.time(sampleIndex), animationState.speed(sampleIndex));
    groundVertices = flightHorizonGround(...
      animationState.roll(sampleIndex), animationState.pitch(sampleIndex));
    lineVertices = flightHorizonLine(...
      animationState.roll(sampleIndex), animationState.pitch(sampleIndex));
    set(animationState.horizonGround, ...
      'XData', groundVertices(:, 1), 'YData', groundVertices(:, 2), ...
      'ZData', -ones(size(groundVertices, 1), 1));
    set(animationState.horizonLine, ...
      'XData', lineVertices(:, 1), 'YData', lineVertices(:, 2));
    title(animationState.flightAxes, ...
      sprintf('Flight time: %.1f s', animationState.time(sampleIndex)));
    set(animationState.horizonStatus, 'String', ...
      sprintf('Roll %+.1f deg   Pitch %+.1f deg', ...
      animationState.roll(sampleIndex) * 180 / pi, ...
      animationState.pitch(sampleIndex) * 180 / pi));
  catch exception
    keepAnimating = false;
    if flightAnimationHandlesValid(animationState) && ...
      ~ contains(exception.message, 'Invalid graphics object')
      rethrow(exception);
    end
  end
end
%=============================================================================
function valid = flightAnimationHandlesValid(animationState)
  valid = isgraphics(animationState.figure) && ...
    isgraphics(animationState.aircraft) && ...
    isgraphics(animationState.flightAxes) && ...
    isgraphics(animationState.telemetryAxes) && ...
    isgraphics(animationState.horizonAxes) && ...
    isgraphics(animationState.trail) && ...
    isgraphics(animationState.altitudeTrace) && ...
    isgraphics(animationState.speedTrace) && ...
    isgraphics(animationState.horizonGround) && ...
    isgraphics(animationState.horizonLine) && ...
    isgraphics(animationState.horizonStatus);
end
%=============================================================================
function cleared = flightClearAnimation(animationState)
  cleared = flightAnimationHandlesValid(animationState);
  if ~cleared
    return
  end
  try
    clearpoints(animationState.trail);
    clearpoints(animationState.altitudeTrace);
    clearpoints(animationState.speedTrace);
  catch exception
    cleared = false;
    if flightAnimationHandlesValid(animationState) && ...
      ~ contains(exception.message, 'Invalid graphics object')
      rethrow(exception);
    end
  end
end
%=============================================================================
