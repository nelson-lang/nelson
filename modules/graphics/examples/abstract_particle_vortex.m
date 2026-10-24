%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a relaxing abstract vortex made of colored particles.
if ~exist('vortexFigureVisible', 'var')
  vortexFigureVisible = 'on';
end
if ~exist('vortexBatch', 'var')
  vortexBatch = false;
end
if ~exist('vortexParticleCount', 'var')
  vortexParticleCount = 1800;
end
if ~exist('vortexFrameCount', 'var')
  vortexFrameCount = 900;
end
if ~exist('vortexMaxFrames', 'var')
  vortexMaxFrames = vortexFrameCount;
end
if ~exist('vortexEnableAudio', 'var')
  vortexEnableAudio = ~vortexBatch;
end

targetFrameRate = 60;
targetFrameDuration = 1 / targetFrameRate;
vortexRadius = 1.0;
[radius, angle, depth, phase, lane] = vortexInitialParticles(vortexParticleCount);
[x, y, markerSize, particleColor, glowSize, glowColor, highlightColor, ...
  tailX, tailY] = vortexFrameParticles(radius, angle, depth, phase, lane, 0, ...
  vortexRadius);
[rayX, rayY] = vortexLightRays(0);
highlightX = x - 0.010;
highlightY = y + 0.010;
shadowX = x + 0.014;
shadowY = y - 0.016;

figureHandle = figure('Name', 'Abstract particle vortex', ...
  'NumberTitle', 'off', 'Color', [0.005 0.007 0.014], ...
  'Visible', vortexFigureVisible);
axesHandle = axes('Parent', figureHandle, 'Color', [0.005 0.007 0.014], ...
  'Position', [0 0 1 1]);
hold(axesHandle, 'on');
vortexDrawBackground(axesHandle);
rayHandle = plot(axesHandle, rayX, rayY, 'Color', [0.10 0.23 0.50], ...
  'LineWidth', 1.2);
shadowHandle = scatter(axesHandle, shadowX, shadowY, 1.10 * markerSize, ...
  [0.015 0.018 0.030], 's', 'filled');
glowHandle = scatter(axesHandle, x, y, glowSize, glowColor, 's', 'filled');
tailHandle = plot(axesHandle, tailX, tailY, 'Color', [0.36 0.62 0.94], ...
  'LineWidth', 0.9);
particleHandle = scatter(axesHandle, x, y, markerSize, particleColor, ...
  's', 'filled');
highlightHandle = scatter(axesHandle, highlightX, highlightY, ...
  0.30 * markerSize, highlightColor, 's', 'filled');
coreHandle = scatter(axesHandle, 0, 0, 160, [0.95 0.78 0.46], 'filled');
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
axis(axesHandle, [-1.16 1.16 -1.16 1.16]);
axis(axesHandle, 'off');
set(axesHandle, 'YDir', 'normal');

vortexAudioPlayer = vortexStartAudio(vortexEnableAudio, vortexFrameCount / ...
  targetFrameRate);
[xHistory, yHistory, depthHistory] = vortexRunAnimation(figureHandle, ...
  particleHandle, glowHandle, highlightHandle, shadowHandle, tailHandle, ...
  rayHandle, coreHandle, radius, angle, depth, phase, lane, vortexRadius, ...
  targetFrameDuration, vortexBatch, vortexFrameCount, vortexMaxFrames);
vortexStopAudio(vortexAudioPlayer);
%=============================================================================
function [radius, angle, depth, phase, lane] = vortexInitialParticles(particleCount)
  rng(27);
  laneCount = 9;
  lane = mod((0:particleCount - 1)', laneCount);
  depth = rand(particleCount, 1);
  radius = 0.045 + 0.955 * rand(particleCount, 1) .^ 0.68;
  laneAngle = 2 * pi * lane / laneCount;
  angle = laneAngle + 9.1 * radius + 0.22 * randn(particleCount, 1);
  phase = 2 * pi * rand(particleCount, 1);
end
%=============================================================================
function [x, y, markerSize, color, glowSize, glowColor, highlightColor, tailX, ...
  tailY] = vortexFrameParticles(radius, angle, depth, phase, lane, frameIndex, ...
  vortexRadius)
  time = frameIndex / 60;
  laneWake = 0.12 * sin(0.55 * lane + 0.45 * time);
  swirl = angle + 1.18 * time + 4.2 * (1 - radius) + laneWake + ...
    0.15 * sin(phase + 1.3 * time);
  schooling = 0.018 * sin(2.8 * lane + phase + 0.9 * time);
  breathing = 0.026 * sin(phase + 0.8 * time) + schooling;
  projectedRadius = vortexRadius * radius .* (0.52 + 0.48 * depth) + breathing;
  x = projectedRadius .* cos(swirl);
  y = 0.72 * projectedRadius .* sin(swirl);
  y = y + 0.10 * sin(2.0 * swirl + 1.1 * time) .* (1 - radius);
  glow = (1 - radius) .* (0.36 + 0.64 * depth);
  color = vortexParticleColors(glow, depth, phase, time);
  markerSize = 9 + 38 * glow .^ 1.4 + 7 * depth;
  glowSize = 3.6 * markerSize + 28 * glow;
  glowColor = 0.46 * color + 0.06;
  highlightColor = min(1, 0.74 * color + 0.32);
  [tailX, tailY] = vortexParticleTails(x, y, swirl, radius, depth);
end
%=============================================================================
function [rayX, rayY] = vortexLightRays(frameIndex)
  rayCount = 14;
  pointsPerRay = 36;
  time = frameIndex / 60;
  rayX = NaN((pointsPerRay + 1) * rayCount, 1);
  rayY = rayX;
  cursor = 1;
  r = linspace(0.05, 1.08, pointsPerRay)';
  for rayIndex = 1:rayCount
    baseAngle = 2 * pi * (rayIndex - 1) / rayCount + 0.16 * time;
    angle = baseAngle + 1.7 * (1 - r) + 0.08 * sin(5 * r + time);
    span = cursor:cursor + pointsPerRay - 1;
    rayX(span) = r .* cos(angle);
    rayY(span) = 0.72 * r .* sin(angle);
    cursor = cursor + pointsPerRay + 1;
  end
end
%=============================================================================
function [tailX, tailY] = vortexParticleTails(x, y, swirl, radius, depth)
  tangentX = -sin(swirl);
  tangentY = 0.72 * cos(swirl);
  tangentLength = sqrt(tangentX .^ 2 + tangentY .^ 2 + eps);
  tangentX = tangentX ./ tangentLength;
  tangentY = tangentY ./ tangentLength;
  tailLength = 0.026 + 0.075 * (1 - radius) .* (0.35 + 0.65 * depth);
  tailX = [x - tailLength .* tangentX, x, NaN(size(x))]';
  tailY = [y - tailLength .* tangentY, y, NaN(size(y))]';
  tailX = tailX(:);
  tailY = tailY(:);
end
%=============================================================================
function color = vortexParticleColors(glow, depth, phase, time)
  wave = 0.5 + 0.5 * sin(phase + 0.85 * time);
  color = zeros(numel(glow), 3);
  color(:, 1) = 0.22 + 0.68 * glow + 0.22 * wave;
  color(:, 2) = 0.24 + 0.52 * depth + 0.28 * (1 - wave);
  color(:, 3) = 0.34 + 0.52 * (1 - glow) + 0.20 * wave;
  color(color > 1) = 1;
end
%=============================================================================
function vortexDrawBackground(axesHandle)
  imageSize = 220;
  coordinates = linspace(-1.16, 1.16, imageSize);
  [xGrid, yGrid] = meshgrid(coordinates, coordinates);
  distance = sqrt(xGrid .^ 2 + (1.28 * yGrid) .^ 2);
  halo = exp(-3.0 * distance .^ 2);
  center = exp(-22.0 * distance .^ 2);
  background = zeros(imageSize, imageSize, 3);
  background(:, :, 1) = 0.006 + 0.18 * halo + 0.28 * center;
  background(:, :, 2) = 0.008 + 0.12 * halo + 0.17 * center;
  background(:, :, 3) = 0.018 + 0.28 * halo + 0.10 * center;
  image('Parent', axesHandle, 'XData', [-1.16 1.16], ...
    'YData', [-1.16 1.16], 'CData', background);
end
%=============================================================================
function player = vortexStartAudio(enableAudio, duration)
  player = [];
  if ~enableAudio || exist('audioplayer', 'builtin') == 0 || ...
      exist('audiodevinfo', 'builtin') == 0
    return
  end
  try
    devices = audiodevinfo();
    if isempty(devices.output)
      return
    end
    fs = 22050;
    samples = vortexAmbientAudio(max(duration, 12), fs);
    player = audioplayer(samples, fs);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function vortexStopAudio(player)
  if isempty(player)
    return
  end
  try
    if isvalid(player)
      stop(player);
    end
  catch
  end
end
%=============================================================================
function samples = vortexAmbientAudio(duration, fs)
  t = (0:round(duration * fs) - 1)' / fs;
  chord = [146.83 196.00 246.94 329.63];
  left = zeros(size(t));
  right = zeros(size(t));
  for toneIndex = 1:numel(chord)
    freq = chord(toneIndex);
    drift = 0.45 * sin(2 * pi * (0.018 + 0.004 * toneIndex) * t);
    phase = 2 * pi * cumsum((freq + drift) / fs);
    voice = sin(phase) + 0.38 * sin(2 * phase + toneIndex);
    swell = 0.52 + 0.48 * sin(2 * pi * (0.035 + 0.006 * toneIndex) * t + ...
      toneIndex);
    pan = 0.5 + 0.38 * sin(2 * pi * (0.021 + 0.003 * toneIndex) * t + ...
      0.7 * toneIndex);
    left = left + voice .* swell .* (1.1 - pan);
    right = right + voice .* swell .* (0.7 + pan);
  end
  shimmer = sin(2 * pi * (587.33 + 1.6 * sin(2 * pi * 0.027 * t)) .* t);
  shimmer = shimmer .* (0.25 + 0.75 * sin(2 * pi * 0.061 * t) .^ 2);
  left = left + 0.10 * shimmer;
  right = right + 0.08 * circshift(shimmer, round(0.011 * fs));
  envelope = min(1, t / 2.5) .* min(1, (duration - t) / 2.5);
  samples = 0.16 * [left right] .* envelope;
  peak = max(abs(samples(:)));
  if peak > 0
    samples = 0.82 * samples / peak;
  end
end
%=============================================================================
function [xHistory, yHistory, depthHistory] = vortexRunAnimation(...
  figureHandle, particleHandle, glowHandle, highlightHandle, shadowHandle, ...
  tailHandle, rayHandle, coreHandle, radius, angle, depth, phase, lane, ...
  vortexRadius, targetFrameDuration, vortexBatch, frameCount, maxFrames)
  renderedFrameCount = min(frameCount, maxFrames);
  particleCount = numel(radius);
  xHistory = zeros(particleCount, frameCount);
  yHistory = zeros(particleCount, frameCount);
  depthHistory = zeros(particleCount, frameCount);
  for frameIndex = 1:renderedFrameCount
    if ~isgraphics(figureHandle) || ~isgraphics(particleHandle) || ...
        ~isgraphics(glowHandle) || ~isgraphics(highlightHandle) || ...
        ~isgraphics(shadowHandle) || ~isgraphics(tailHandle) || ...
        ~isgraphics(rayHandle)
      break
    end
    frameTimer = tic();
    depth = depth + 0.0065 + 0.0018 * (1 - radius);
    wrap = depth > 1;
    depth(wrap) = depth(wrap) - 1;
    angle(wrap) = angle(wrap) + 0.45 + 0.12 * sin(phase(wrap));
    [x, y, markerSize, particleColor, glowSize, glowColor, highlightColor, ...
      tailX, tailY] = vortexFrameParticles(radius, angle, depth, phase, lane, ...
      frameIndex, vortexRadius);
    [rayX, rayY] = vortexLightRays(frameIndex);
    highlightX = x - 0.010;
    highlightY = y + 0.010;
    shadowX = x + 0.014;
    shadowY = y - 0.016;
    xHistory(:, frameIndex) = x;
    yHistory(:, frameIndex) = y;
    depthHistory(:, frameIndex) = depth;
    set(rayHandle, 'XData', rayX, 'YData', rayY);
    set(glowHandle, 'XData', x, 'YData', y, 'SizeData', glowSize, ...
      'CData', glowColor);
    set(shadowHandle, 'XData', shadowX, 'YData', shadowY, ...
      'SizeData', 1.10 * markerSize);
    set(particleHandle, 'XData', x, 'YData', y, 'SizeData', markerSize, ...
      'CData', particleColor);
    set(highlightHandle, 'XData', highlightX, 'YData', highlightY, ...
      'SizeData', 0.30 * markerSize, 'CData', highlightColor);
    set(tailHandle, 'XData', tailX, 'YData', tailY);
    coreColor = [0.86 + 0.08 * sin(frameIndex / 27), 0.66, 0.42];
    set(coreHandle, 'CData', coreColor, ...
      'SizeData', 138 + 28 * sin(frameIndex / 19));
    drawnow();
    if ~vortexBatch
      pause(max(0, targetFrameDuration - toc(frameTimer)));
    end
  end
end
%=============================================================================
