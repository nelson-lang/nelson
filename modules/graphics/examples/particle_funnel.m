%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate soft particles falling through a funnel with paced rendering.
if ~exist('funnelFigureVisible', 'var')
  funnelFigureVisible = 'on';
end
if ~exist('funnelBatch', 'var')
  funnelBatch = false;
end
if ~exist('funnelParticleCount', 'var')
  funnelParticleCount = 500;
end
if ~exist('funnelFrameCount', 'var')
  funnelFrameCount = 540;
end
if ~exist('funnelMaxFrames', 'var')
  funnelMaxFrames = funnelFrameCount;
end
if ~exist('funnelOutputImage', 'var')
  funnelOutputImage = '';
end

targetFrameRate = 60;
targetFrameDuration = 1 / targetFrameRate;
physicsStepsPerFrame = 1;
particleRadius = 0.050;
funnelTimeStep = 1 / (targetFrameRate * physicsStepsPerFrame);

[particleX, particleY] = funnelInitialParticles(funnelParticleCount);
[previousParticleX, previousParticleY] = ...
  funnelInitialPreviousParticles(particleX, particleY);
[funnelX, funnelY] = funnelWallPolyline();
[objectX, objectY, objectRadius] = funnelObstacleObjects();

baseHue = linspace(0, 1, funnelParticleCount)';
particleColor = funnelParticleColors(baseHue, zeros(funnelParticleCount, 1), ...
  zeros(funnelParticleCount, 1));
backgroundImage = funnelBackgroundImage();
particleMarkerSize = max(10, min(42, 460 / sqrt(funnelParticleCount)));
shadowMarkerSize = max(8, 0.86 * particleMarkerSize);
figurePosition = funnelFigurePosition();

figureHandle = figure('Name', 'Particle funnel', ...
  'NumberTitle', 'off', 'Color', [0.035 0.045 0.070], ...
  'Visible', funnelFigureVisible, 'Position', figurePosition);
axesHandle = axes('Parent', figureHandle, 'Position', [0.05 0.06 0.90 0.88]);
hold(axesHandle, 'on');
image('Parent', axesHandle, 'XData', [-3.2 3.2], 'YData', [0 8.2], ...
  'CData', backgroundImage);
patch('Parent', axesHandle, 'XData', [-2.9 2.9 2.9 -2.9], ...
  'YData', [0.08 0.08 0.34 0.34], 'FaceColor', [0.12 0.14 0.18], ...
  'EdgeColor', [0.35 0.39 0.48], 'LineWidth', 1.4);
funnelDrawObstacleObjects(axesHandle, objectX, objectY, objectRadius);
plot(axesHandle, funnelX, funnelY, 'Color', [0.94 0.76 0.38], ...
  'LineWidth', 4.0);
plot(axesHandle, funnelX, funnelY, 'Color', [1.00 0.96 0.72], ...
  'LineWidth', 1.2);
shadowHandle = scatter(axesHandle, particleX + 0.035, ...
  particleY - 0.045, shadowMarkerSize, [0 0 0], 'filled');
particleHandle = scatter(axesHandle, particleX, particleY, ...
  particleMarkerSize, particleColor, 'filled');
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
axis(axesHandle, [-3.05 3.05 0 8.05]);
set(axesHandle, 'YDir', 'normal', 'XColor', [0.62 0.68 0.78], ...
  'YColor', [0.62 0.68 0.78], 'Color', [0.035 0.045 0.070]);
set(axesHandle, 'XTick', [], 'YTick', []);
title(axesHandle, 'Soft particle funnel', ...
  'Color', [0.95 0.96 0.99]);
statusText = text(axesHandle, -2.82, 7.75, '', ...
  'Color', [0.88 0.92 1.00], 'FontSize', 11);
startStopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Stop', 'Position', [18 18 86 30], 'UserData', 'none', ...
  'Callback', @funnelStartStopRequest);

[particleXHistory, particleYHistory, speedHistory, impactHistory] = ...
  funnelRunAnimation(startStopButton, particleHandle, shadowHandle, statusText, ...
  particleX, particleY, previousParticleX, previousParticleY, ...
  objectX, objectY, objectRadius, baseHue, targetFrameDuration, funnelBatch, ...
  funnelParticleCount, funnelFrameCount, funnelMaxFrames, funnelTimeStep, ...
  particleRadius);
if ~isempty(funnelOutputImage) && isgraphics(figureHandle)
  drawnow();
  saveas(figureHandle, funnelOutputImage);
end
%=============================================================================
function [x, y] = funnelInitialParticles(particleCount)
  rng(13);
  columnCount = ceil(sqrt(1.35 * particleCount));
  rowCount = ceil(particleCount / columnCount);
  baseX = linspace(-2.05, 2.05, columnCount);
  baseY = linspace(5.82, 7.64, rowCount);
  [gridX, gridY] = meshgrid(baseX, baseY);
  x = gridX(:);
  y = gridY(:);
  x = x(1:particleCount) + 0.012 * rand(particleCount, 1);
  y = y(1:particleCount) + 0.012 * rand(particleCount, 1);
end
%=============================================================================
function [previousX, previousY] = funnelInitialPreviousParticles(x, y)
  particleCount = numel(x);
  previousX = x - 0.012 * sin((1:particleCount)' * 0.73);
  previousY = y + 0.006;
end
%=============================================================================
function [funnelX, funnelY] = funnelWallPolyline()
  funnelX = [-2.45 -0.48 -0.48 -2.75 NaN 2.45 0.48 0.48 2.75];
  funnelY = [7.42 2.72 0.34 0.34 NaN 7.42 2.72 0.34 0.34];
end
%=============================================================================
function [objectX, objectY, objectRadius] = funnelObstacleObjects()
  objectX = [-1.25; 1.20; -0.56; 0.55; 0.00];
  objectY = [5.95; 5.78; 4.70; 4.42; 3.42];
  objectRadius = [0.18; 0.18; 0.16; 0.16; 0.15];
end
%=============================================================================
function funnelDrawObstacleObjects(axesHandle, objectX, objectY, objectRadius)
  angle = linspace(0, 2 * pi, 50);
  objectColors = [0.94 0.30 0.62; 0.33 0.86 0.95; 0.98 0.68 0.28; ...
    0.58 0.86 0.30; 0.82 0.54 0.96];
  for objectIndex = 1:numel(objectX)
    x = objectX(objectIndex) + objectRadius(objectIndex) * cos(angle);
    y = objectY(objectIndex) + objectRadius(objectIndex) * sin(angle);
    patch('Parent', axesHandle, 'XData', x + 0.035, 'YData', y - 0.045, ...
      'FaceColor', [0 0 0], 'EdgeColor', 'none');
    patch('Parent', axesHandle, 'XData', x, 'YData', y, ...
      'FaceColor', objectColors(objectIndex, :), ...
      'EdgeColor', [1.00 0.96 0.88], 'LineWidth', 1.1);
  end
end
%=============================================================================
function figurePosition = funnelFigurePosition()
  screenSize = get(0, 'ScreenSize');
  if numel(screenSize) < 4 || screenSize(3) < 640 || screenSize(4) < 520
    figurePosition = [90 70 900 760];
    return
  end
  margin = 72;
  figureWidth = max(720, screenSize(3) - 2 * margin);
  figureHeight = max(560, screenSize(4) - 2 * margin);
  figureWidth = min(figureWidth, screenSize(3) - 24);
  figureHeight = min(figureHeight, screenSize(4) - 48);
  figureLeft = screenSize(1) + (screenSize(3) - figureWidth) / 2;
  figureBottom = screenSize(2) + (screenSize(4) - figureHeight) / 2;
  figurePosition = [figureLeft figureBottom figureWidth figureHeight];
end
%=============================================================================
function imageData = funnelBackgroundImage()
  imageHeight = 180;
  imageWidth = 160;
  vertical = linspace(0, 1, imageHeight)';
  horizontal = linspace(-1, 1, imageWidth);
  [xGrid, yGrid] = meshgrid(horizontal, vertical);
  glow = exp(-2.7 * xGrid .^ 2) .* (0.28 + 0.72 * yGrid);
  imageData = zeros(imageHeight, imageWidth, 3);
  imageData(:, :, 1) = 0.035 + 0.100 * glow + 0.035 * yGrid;
  imageData(:, :, 2) = 0.045 + 0.145 * glow + 0.020 * yGrid;
  imageData(:, :, 3) = 0.070 + 0.220 * glow + 0.070 * yGrid;
end
%=============================================================================
function color = funnelParticleColors(baseHue, speed, impact)
  speedScale = min(1, speed / max(1.0, max(speed)));
  impactScale = min(1, impact / max(0.5, max(impact)));
  color = zeros(numel(baseHue), 3);
  color(:, 1) = 0.88 - 0.40 * baseHue + 0.16 * speedScale + ...
    0.28 * impactScale;
  color(:, 2) = 0.34 + 0.50 * baseHue + 0.18 * impactScale;
  color(:, 3) = 0.14 + 0.42 * (1 - baseHue) + 0.18 * speedScale + ...
    0.30 * impactScale;
  color(color > 1) = 1;
end
%=============================================================================
function funnelStartStopRequest(source, event)
  if strcmp(get(source, 'String'), 'Stop')
    set(source, 'UserData', 'stop');
  else
    set(source, 'UserData', 'start');
  end
end
%=============================================================================
function [xHistory, yHistory, speedHistory, impactHistory] = ...
  funnelRunAnimation(startStopButton, particleHandle, shadowHandle, statusText, ...
  initialX, initialY, initialPreviousX, initialPreviousY, objectX, objectY, objectRadius, ...
  baseHue, targetFrameDuration, funnelBatch, particleCount, frameCount, ...
  maxFrames, timeStep, particleRadius)
  renderedFrameCount = min(maxFrames, frameCount);
  xHistory = zeros(particleCount, frameCount);
  yHistory = zeros(particleCount, frameCount);
  speedHistory = zeros(particleCount, frameCount);
  impactHistory = zeros(particleCount, frameCount);
  x = initialX;
  y = initialY;
  previousX = initialPreviousX;
  previousY = initialPreviousY;
  running = true;
  frameIndex = 0;
  while isgraphics(particleHandle) && isgraphics(shadowHandle)
    drawnow();
    [running, x, y, previousX, previousY, frameIndex] = ...
      funnelApplyControlRequest(startStopButton, running, x, y, previousX, ...
      previousY, initialX, initialY, initialPreviousX, initialPreviousY, ...
      frameIndex);
    if ~running
      if funnelBatch
        break
      end
      pause(0.030);
      continue
    end
    if frameIndex >= renderedFrameCount
      running = false;
      funnelSetButtonState(startStopButton, 'Start');
      if funnelBatch
        break
      end
      continue
    end
    frameIndex = frameIndex + 1;
    frameTimer = tic();
    [x, y, previousX, previousY, speed, impact] = funnelStepParticles(...
      x, y, previousX, previousY, timeStep, particleRadius, objectX, ...
      objectY, objectRadius);
    xHistory(:, frameIndex) = x;
    yHistory(:, frameIndex) = y;
    speedHistory(:, frameIndex) = speed;
    impactHistory(:, frameIndex) = impact;
    particleColor = funnelParticleColors(baseHue, speed, impact);
    set(shadowHandle, 'XData', x + 0.035, ...
      'YData', max(0.34, y - 0.045));
    set(particleHandle, 'XData', x, 'YData', y, 'CData', particleColor);
    set(statusText, 'String', sprintf('%3d particles - frame %03d/%03d', ...
      particleCount, frameIndex, frameCount));
    drawnow();
    if ~funnelBatch
      funnelPauseResponsive(startStopButton, targetFrameDuration - toc(frameTimer));
    end
  end
  funnelSetButtonState(startStopButton, 'Start');
end
%=============================================================================
function [running, x, y, previousX, previousY, frameIndex] = ...
  funnelApplyControlRequest(startStopButton, running, x, y, previousX, ...
  previousY, initialX, initialY, initialPreviousX, initialPreviousY, ...
  frameIndex)
  if ~isgraphics(startStopButton)
    return
  end
  request = get(startStopButton, 'UserData');
  if strcmp(request, 'none')
    return
  end
  set(startStopButton, 'UserData', 'none');
  if strcmp(request, 'stop')
    running = false;
    funnelSetButtonState(startStopButton, 'Start');
    return
  end
  x = initialX;
  y = initialY;
  previousX = initialPreviousX;
  previousY = initialPreviousY;
  frameIndex = 0;
  running = true;
  funnelSetButtonState(startStopButton, 'Stop');
end
%=============================================================================
function funnelSetButtonState(startStopButton, label)
  if isgraphics(startStopButton)
    set(startStopButton, 'String', label);
  end
end
%=============================================================================
function funnelPauseResponsive(startStopButton, duration)
  remaining = max(0, duration);
  while remaining > 0
    pause(min(remaining, 0.004));
    drawnow();
    if isgraphics(startStopButton) && strcmp(get(startStopButton, 'UserData'), 'stop')
      return
    end
    remaining = remaining - 0.004;
  end
end
%=============================================================================
function [x, y, previousX, previousY, speed, impact] = funnelStepParticles(...
  x, y, previousX, previousY, timeStep, particleRadius, objectX, objectY, ...
  objectRadius)
  velocityX = (x - previousX) / timeStep;
  velocityY = (y - previousY) / timeStep;
  accelerationX = 0.10 * sin(2.1 * y + 0.7 * x);
  accelerationY = -2.25 * ones(size(y));
  impact = zeros(size(y));
  [accelerationX, accelerationY, impact] = funnelParticleContactForces(...
    x, y, velocityX, velocityY, particleRadius, accelerationX, accelerationY);
  [accelerationX, accelerationY, impact] = funnelWallForces(...
    x, y, velocityX, velocityY, particleRadius, accelerationX, accelerationY, ...
    impact);
  [accelerationX, accelerationY, impact] = funnelObstacleForces(...
    x, y, velocityX, velocityY, particleRadius, objectX, objectY, ...
    objectRadius, accelerationX, accelerationY, impact);
  nextX = x + 0.996 * (x - previousX) + accelerationX * timeStep ^ 2;
  nextY = y + 0.996 * (y - previousY) + accelerationY * timeStep ^ 2;
  previousX = x;
  previousY = y;
  x = nextX;
  y = nextY;
  speed = hypot((x - previousX) / timeStep, (y - previousY) / timeStep);
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelObstacleForces(...
  x, y, velocityX, velocityY, radius, objectX, objectY, objectRadius, ...
  accelerationX, accelerationY, impact)
  for objectIndex = 1:numel(objectX)
    deltaX = x - objectX(objectIndex);
    deltaY = y - objectY(objectIndex);
    distance = sqrt(deltaX .^ 2 + deltaY .^ 2 + 1e-12);
    contactDistance = radius + objectRadius(objectIndex);
    overlap = contactDistance - distance;
    contact = overlap > 0;
    normalVelocity = (velocityX .* deltaX + velocityY .* deltaY) ./ distance;
    force = contact .* (1220 * overlap - 34 * min(normalVelocity, 0));
    accelerationX = accelerationX + force .* deltaX ./ distance;
    accelerationY = accelerationY + force .* deltaY ./ distance;
    impact = impact + contact .* max(-normalVelocity, 0);
  end
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelParticleContactForces(...
  x, y, velocityX, velocityY, radius, accelerationX, accelerationY)
  deltaX = x - x';
  deltaY = y - y';
  distance = sqrt(deltaX .^ 2 + deltaY .^ 2 + 1e-12);
  overlap = 2 * radius - distance;
  contact = overlap > 0;
  contact(1:numel(x) + 1:end) = false;
  normalVelocity = ((velocityX - velocityX') .* deltaX + ...
    (velocityY - velocityY') .* deltaY) ./ distance;
  force = contact .* (760 * overlap - 24 * min(normalVelocity, 0));
  accelerationX = accelerationX + sum(force .* deltaX ./ distance, 2);
  accelerationY = accelerationY + sum(force .* deltaY ./ distance, 2);
  impact = sum(contact .* max(-normalVelocity, 0), 2);
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelWallForces(...
  x, y, velocityX, velocityY, radius, accelerationX, accelerationY, impact)
  [accelerationX, accelerationY, impact] = funnelLineForce(x, y, velocityX, ...
    velocityY, radius, [-2.45 7.42], [-0.48 2.72], [4.72 1.97], ...
    y > 2.55, accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelLineForce(x, y, velocityX, ...
    velocityY, radius, [2.45 7.42], [0.48 2.72], [-4.72 1.97], ...
    y > 2.55, accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelVerticalForce(x, velocityX, ...
    radius, -0.48, 1, y <= 2.80, accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelVerticalForce(x, velocityX, ...
    radius, 0.48, -1, y <= 2.80, accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelHorizontalForce(y, velocityY, ...
    radius, 0.34, 1, accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelVerticalForce(x, velocityX, ...
    radius, -2.80, 1, true(size(x)), accelerationX, accelerationY, impact);
  [accelerationX, accelerationY, impact] = funnelVerticalForce(x, velocityX, ...
    radius, 2.80, -1, true(size(x)), accelerationX, accelerationY, impact);
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelLineForce(x, y, velocityX, ...
  velocityY, radius, pointA, pointB, normal, active, accelerationX, ...
  accelerationY, impact)
  normal = normal / hypot(normal(1), normal(2));
  signedDistance = (x - pointA(1)) * normal(1) + ...
    (y - pointA(2)) * normal(2);
  along = ((x - pointA(1)) * (pointB(1) - pointA(1)) + ...
    (y - pointA(2)) * (pointB(2) - pointA(2))) / ...
    ((pointB(1) - pointA(1)) ^ 2 + (pointB(2) - pointA(2)) ^ 2);
  contact = active & along >= -0.03 & along <= 1.03 & signedDistance < radius;
  normalVelocity = velocityX * normal(1) + velocityY * normal(2);
  force = contact .* (980 * (radius - signedDistance) - ...
    28 * min(normalVelocity, 0));
  accelerationX = accelerationX + force * normal(1);
  accelerationY = accelerationY + force * normal(2);
  impact = impact + contact .* max(-normalVelocity, 0);
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelVerticalForce(x, ...
  velocityX, radius, wallX, normalX, active, accelerationX, accelerationY, ...
  impact)
  signedDistance = (x - wallX) * normalX;
  contact = active & signedDistance < radius;
  normalVelocity = velocityX * normalX;
  force = contact .* (1050 * (radius - signedDistance) - ...
    30 * min(normalVelocity, 0));
  accelerationX = accelerationX + force * normalX;
  impact = impact + contact .* max(-normalVelocity, 0);
end
%=============================================================================
function [accelerationX, accelerationY, impact] = funnelHorizontalForce(y, ...
  velocityY, radius, wallY, normalY, accelerationX, accelerationY, impact)
  signedDistance = (y - wallY) * normalY;
  contact = signedDistance < radius;
  normalVelocity = velocityY * normalY;
  force = contact .* (1150 * (radius - signedDistance) - ...
    32 * min(normalVelocity, 0));
  accelerationY = accelerationY + force * normalY;
  impact = impact + contact .* max(-normalVelocity, 0);
end
%=============================================================================
