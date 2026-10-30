%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Drive a particle fountain entirely from a JSON scene decoded with jsondecode.
if ~exist('jsonParticlesFigureVisible', 'var')
  jsonParticlesFigureVisible = 'on';
end
if ~exist('jsonParticlesAnimationEnabled', 'var')
  jsonParticlesAnimationEnabled = true;
end
if ~exist('jsonParticlesAnimationFrames', 'var')
  jsonParticlesAnimationFrames = 1200;
end
if ~exist('jsonParticlesFramePause', 'var')
  jsonParticlesFramePause = 0;
end
if ~exist('jsonParticlesAnimationContinuous', 'var')
  jsonParticlesAnimationContinuous = ...
    strcmp(jsonParticlesFigureVisible, 'on');
end

% The whole scene is data: edit this JSON and the fountain changes.
if ~exist('jsonParticlesScene', 'var')
  jsonParticlesScene = ['{', ...
    '"gravity": -0.012, "drag": 0.995, "spawnPerFrame": 3,', ...
    '"emitters": [', ...
    '{"x": 0.30, "y": 0.05, "speed": 0.045, "spread": 0.5,', ...
    ' "color": [0.95, 0.42, 0.30]},', ...
    '{"x": 0.50, "y": 0.05, "speed": 0.055, "spread": 0.4,', ...
    ' "color": [0.35, 0.85, 1.00]},', ...
    '{"x": 0.70, "y": 0.05, "speed": 0.050, "spread": 0.5,', ...
    ' "color": [0.98, 0.80, 0.30]}]}'];
end
jsonParticlesConfig = jsondecode(jsonParticlesScene);
jsonParticlesEmitters = jsonParticlesConfig.emitters;
jsonParticlesEmitterCount = numel(jsonParticlesEmitters);
rng(12345);

jsonParticlesFigure = figure('Name', 'JSON particle fountain', ...
  'NumberTitle', 'off', 'Color', [0.03 0.03 0.06], ...
  'Visible', jsonParticlesFigureVisible, 'Position', [120 90 820 820]);
jsonParticlesAxes = axes('Parent', jsonParticlesFigure, ...
  'Position', [0.03 0.05 0.94 0.90], 'Color', [0.03 0.03 0.06]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(jsonParticlesAxes);
hold(jsonParticlesAxes, 'on');
jsonParticlesScatter = gobjects(1, jsonParticlesEmitterCount);
jsonParticlesState = cell(1, jsonParticlesEmitterCount);
for emitterIndex = 1:jsonParticlesEmitterCount
  color = jsonParticlesEmitters(emitterIndex).color(:).';
  jsonParticlesScatter(emitterIndex) = plot(jsonParticlesAxes, nan, nan, ...
    '.', 'Color', color, 'MarkerSize', 9, 'LineStyle', 'none');
  jsonParticlesState{emitterIndex} = zeros(0, 5);
end
hold(jsonParticlesAxes, 'off');
axis(jsonParticlesAxes, 'off');
set(jsonParticlesAxes, 'XLim', [0 1], 'YLim', [0 1]);
title(jsonParticlesAxes, 'Particle fountain described by a JSON scene', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
jsonParticlesInfo = uicontrol('Parent', jsonParticlesFigure, 'Style', ...
  'text', 'String', '', 'Position', [16 8 780 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.03 0.03 0.06], ...
  'ForegroundColor', [0.74 0.82 0.95], 'FontSize', 10);

if jsonParticlesAnimationEnabled
  jsonParticlesFrame = 0;
  while isgraphics(jsonParticlesFigure)
    jsonParticlesFrame = jsonParticlesFrame + 1;
    if jsonParticlesFrame > jsonParticlesAnimationFrames && ...
        ~jsonParticlesAnimationContinuous
      break
    end
    if ~isgraphics(jsonParticlesFigure)
      break
    end
    try
      liveCount = 0;
      for emitterIndex = 1:jsonParticlesEmitterCount
        emitter = jsonParticlesEmitters(emitterIndex);
        particles = jsonParticlesState{emitterIndex};
        particles = jsonParticlesSpawn(particles, emitter, ...
          jsonParticlesConfig.spawnPerFrame);
        particles = jsonParticlesAdvance(particles, ...
          jsonParticlesConfig.gravity, jsonParticlesConfig.drag);
        jsonParticlesState{emitterIndex} = particles;
        set(jsonParticlesScatter(emitterIndex), 'XData', ...
          particles(:, 1).', 'YData', particles(:, 2).');
        liveCount = liveCount + size(particles, 1);
      end
      if isgraphics(jsonParticlesInfo)
        set(jsonParticlesInfo, 'String', sprintf(...
          '%d emitters from JSON | %d live particles | gravity %.3f', ...
          jsonParticlesEmitterCount, liveCount, ...
          jsonParticlesConfig.gravity));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if jsonParticlesFramePause > 0
      pause(jsonParticlesFramePause);
    end
  end
end
%=============================================================================
function particles = jsonParticlesSpawn(particles, emitter, spawnCount)
  % Each row is [x y vx vy life]; new particles start at the emitter mouth.
  born = zeros(spawnCount, 5);
  angles = pi / 2 + emitter.spread * (rand(spawnCount, 1) - 0.5);
  speeds = emitter.speed * (0.6 + 0.8 * rand(spawnCount, 1));
  born(:, 1) = emitter.x + 0.01 * randn(spawnCount, 1);
  born(:, 2) = emitter.y;
  born(:, 3) = cos(angles) .* speeds;
  born(:, 4) = sin(angles) .* speeds;
  born(:, 5) = 1.0;
  particles = [particles; born];
end
%=============================================================================
function particles = jsonParticlesAdvance(particles, gravity, drag)
  if isempty(particles)
    return
  end
  particles(:, 4) = particles(:, 4) + gravity;
  particles(:, 3) = particles(:, 3) * drag;
  particles(:, 1) = particles(:, 1) + particles(:, 3);
  particles(:, 2) = particles(:, 2) + particles(:, 4);
  particles(:, 5) = particles(:, 5) - 0.012;
  % Drop particles that fade out or fall below the floor.
  alive = particles(:, 5) > 0 & particles(:, 2) > 0;
  particles = particles(alive, :);
end
%=============================================================================
