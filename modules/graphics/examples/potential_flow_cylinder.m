%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Advect tracer particles through potential flow past a circular cylinder.
if ~exist('flowFigureVisible', 'var')
  flowFigureVisible = 'on';
end
if ~exist('flowBatch', 'var')
  flowBatch = false;
end
if ~exist('flowMaxFrames', 'var')
  flowMaxFrames = 400;
end
if ~exist('flowFramePause', 'var')
  flowFramePause = 0;
end
if ~exist('flowOutputImage', 'var')
  flowOutputImage = '';
end

cylinderRadius = 0.7;
freeStreamSpeed = 1.2;
circulation = 0.0;
timeStep = 0.03;
domainXMin = -3.0;
domainXMax = 5.0;
domainYMin = -2.6;
domainYMax = 2.6;

laneCount = 44;
columnCount = 13;
laneY = linspace(domainYMin + 0.15, domainYMax - 0.15, laneCount);
columnFraction = ((1:columnCount) - 1) / columnCount;
[columnGrid, laneGrid] = meshgrid(columnFraction, laneY);
homeY = laneGrid(:);
particleX = domainXMin + (domainXMax - domainXMin) * columnGrid(:);
particleY = homeY;
startInside = (particleX .^ 2 + particleY .^ 2) < cylinderRadius ^ 2;
particleX(startInside) = domainXMin;
particleY(startInside) = homeY(startInside);
initialX = particleX;
initialY = particleY;

streamGridX = linspace(domainXMin, domainXMax, 150);
streamGridY = linspace(domainYMin, domainYMax, 110);
[streamMeshX, streamMeshY] = meshgrid(streamGridX, streamGridY);
streamRadiusSquared = streamMeshX .^ 2 + streamMeshY .^ 2;
streamFunction = freeStreamSpeed * ...
  (streamMeshY - cylinderRadius ^ 2 * streamMeshY ./ streamRadiusSquared) + ...
  circulation / (4 * pi) * log(streamRadiusSquared / cylinderRadius ^ 2);
streamFunction(streamRadiusSquared < cylinderRadius ^ 2) = NaN;

% Bake the streamline pattern into a single static raster image so the flow
% background is painted once as one textured quad instead of thousands of
% vector contour segments that would be re-rasterized on every frame.
streamLevels = 16;
finiteStream = streamFunction(~isnan(streamFunction));
streamSpacing = (max(finiteStream) - min(finiteStream)) / streamLevels;
streamPhase = streamFunction / streamSpacing;
streamFraction = abs(streamPhase - round(streamPhase));
streamLineHalfWidth = 0.16;
streamLineMask = max(0, 1 - streamFraction / streamLineHalfWidth);
streamLineMask(isnan(streamLineMask)) = 0;
backgroundColor = [0.08 0.10 0.16];
streamLineColor = [0.34 0.44 0.66];
streamImage = zeros(size(streamFunction, 1), size(streamFunction, 2), 3);
for channel = 1:3
  streamImage(:, :, channel) = backgroundColor(channel) + ...
    streamLineMask * (streamLineColor(channel) - backgroundColor(channel));
end

figureHandle = figure('Name', 'Potential flow past a cylinder', ...
  'NumberTitle', 'off', 'Color', [0.05 0.07 0.12], ...
  'Visible', flowFigureVisible, 'Position', [80 80 1100 660]);
flowAxes = axes('Parent', figureHandle, 'Position', [0.06 0.20 0.90 0.74]);
set(flowAxes, 'Color', [0.08 0.10 0.16]);
hold(flowAxes, 'on');
image('Parent', flowAxes, 'XData', [domainXMin domainXMax], ...
  'YData', [domainYMin domainYMax], 'CData', streamImage);
cylinderAngle = linspace(0, 2 * pi, 160);
cylinderPatch = patch('Parent', flowAxes, ...
  'XData', cylinderRadius * cos(cylinderAngle), ...
  'YData', cylinderRadius * sin(cylinderAngle), ...
  'FaceColor', [0.86 0.32 0.24], 'EdgeColor', [0.98 0.72 0.40], ...
  'LineWidth', 1.5);
particleHandle = plot(flowAxes, particleX, particleY, '.', ...
  'LineStyle', 'none', 'Marker', '.', 'MarkerSize', 9, ...
  'Color', [0.42 0.85 1.00]);
hold(flowAxes, 'off');
axis(flowAxes, 'equal');
axis(flowAxes, [domainXMin domainXMax domainYMin domainYMax]);
set(flowAxes, 'XLimMode', 'manual', 'YLimMode', 'manual', 'YDir', 'normal');
set(flowAxes, 'XColor', [0.70 0.74 0.82], 'YColor', [0.70 0.74 0.82]);
xlabel(flowAxes, 'x');
ylabel(flowAxes, 'y');
title(flowAxes, sprintf('Potential flow   U = %.2f   Gamma = %.2f', ...
  freeStreamSpeed, circulation), 'Color', [0.92 0.94 0.98]);

uicontrol('Parent', figureHandle, 'Style', 'text', ...
  'String', 'Circulation Gamma', 'Position', [30 118 150 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.05 0.07 0.12], ...
  'ForegroundColor', [0.85 0.88 0.94]);
gammaSlider = uicontrol('Parent', figureHandle, 'Style', 'slider', ...
  'Min', -6, 'Max', 6, 'Value', circulation, ...
  'Position', [30 96 320 20]);
uicontrol('Parent', figureHandle, 'Style', 'text', ...
  'String', 'Free-stream speed U', 'Position', [30 66 170 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.05 0.07 0.12], ...
  'ForegroundColor', [0.85 0.88 0.94]);
speedSlider = uicontrol('Parent', figureHandle, 'Style', 'slider', ...
  'Min', 0.3, 'Max', 3.0, 'Value', freeStreamSpeed, ...
  'Position', [30 44 320 20]);
startButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Start', 'Position', [420 84 100 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
stopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Stop', 'Position', [535 84 100 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
resetButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Reset', 'Position', [650 84 100 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));

if flowBatch
  for frameIndex = 1:flowMaxFrames
    if ~isgraphics(figureHandle) || ~isgraphics(particleHandle)
      break
    end
    [particleX, particleY] = flowAdvectParticles(particleX, particleY, ...
      homeY, freeStreamSpeed, circulation, cylinderRadius, timeStep, ...
      domainXMin, domainXMax, domainYMin, domainYMax);
    set(particleHandle, 'XData', particleX, 'YData', particleY);
    drawnow();
    if flowFramePause > 0
      pause(flowFramePause);
    end
  end
  if ~isempty(flowOutputImage) && isgraphics(figureHandle)
    drawnow();
    saveas(figureHandle, flowOutputImage);
  end
else
  flowRunning = true;
  while isgraphics(figureHandle)
    if isgraphics(startButton) && get(startButton, 'UserData')
      flowRunning = true;
      set(startButton, 'UserData', false);
    end
    if isgraphics(stopButton) && get(stopButton, 'UserData')
      flowRunning = false;
      set(stopButton, 'UserData', false);
    end
    if isgraphics(resetButton) && get(resetButton, 'UserData')
      particleX = initialX;
      particleY = initialY;
      set(resetButton, 'UserData', false);
    end
    if isgraphics(speedSlider)
      freeStreamSpeed = get(speedSlider, 'Value');
    end
    if isgraphics(gammaSlider)
      circulation = get(gammaSlider, 'Value');
    end
    if flowRunning
      [particleX, particleY] = flowAdvectParticles(particleX, particleY, ...
        homeY, freeStreamSpeed, circulation, cylinderRadius, timeStep, ...
        domainXMin, domainXMax, domainYMin, domainYMax);
    end
    if ~isgraphics(particleHandle) || ~isgraphics(flowAxes)
      break
    end
    set(particleHandle, 'XData', particleX, 'YData', particleY);
    title(flowAxes, sprintf('Potential flow   U = %.2f   Gamma = %.2f', ...
      freeStreamSpeed, circulation), 'Color', [0.92 0.94 0.98]);
    drawnow();
    pause(flowFramePause);
  end
end
%=============================================================================
function [particleX, particleY] = flowAdvectParticles(particleX, particleY, ...
  homeY, freeStreamSpeed, circulation, cylinderRadius, timeStep, ...
  domainXMin, domainXMax, domainYMin, domainYMax)
  complexPosition = particleX + 1i * particleY;
  velocityConjugate = conj(freeStreamSpeed * ...
    (1 - cylinderRadius ^ 2 ./ complexPosition .^ 2) - ...
    1i * circulation ./ (2 * pi * complexPosition));
  velocityX = real(velocityConjugate);
  velocityY = imag(velocityConjugate);
  inside = abs(complexPosition) < cylinderRadius;
  velocityX(inside) = 0;
  velocityY(inside) = 0;
  particleX = particleX + timeStep * velocityX;
  particleY = particleY + timeStep * velocityY;
  radius = hypot(particleX, particleY);
  respawn = (particleX > domainXMax) | (particleX < domainXMin) | ...
    (particleY > domainYMax) | (particleY < domainYMin) | ...
    (radius < cylinderRadius);
  particleX(respawn) = domainXMin;
  particleY(respawn) = homeY(respawn);
end
%=============================================================================
