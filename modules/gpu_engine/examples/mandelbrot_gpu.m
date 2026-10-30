%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Zoom into the Mandelbrot set, computing each frame on the GPU when available.
if ~exist('mandelbrotFigureVisible', 'var')
  mandelbrotFigureVisible = 'on';
end
if ~exist('mandelbrotAnimationEnabled', 'var')
  mandelbrotAnimationEnabled = true;
end
if ~exist('mandelbrotAnimationFrames', 'var')
  mandelbrotAnimationFrames = 200;
end
if ~exist('mandelbrotFramePause', 'var')
  mandelbrotFramePause = 0;
end
if ~exist('mandelbrotAnimationContinuous', 'var')
  mandelbrotAnimationContinuous = strcmp(mandelbrotFigureVisible, 'on');
end
if ~exist('mandelbrotUseGpu', 'var')
  mandelbrotUseGpu = canUseGPU();
end
if ~exist('mandelbrotResolution', 'var')
  if mandelbrotUseGpu
    mandelbrotResolution = 560;
  else
    mandelbrotResolution = 220;
  end
end
if ~exist('mandelbrotSoundEnabled', 'var')
  mandelbrotSoundEnabled = strcmp(mandelbrotFigureVisible, 'on');
end

mandelbrotMaxIterations = 90;
% A classic seahorse-valley target on the boundary of the set.
mandelbrotCenter = complex(-0.743643887037151, 0.131825904205330);
mandelbrotStartSpan = 3.2;
mandelbrotEndSpan = 0.0008;
mandelbrotFrameCount = max(2, mandelbrotAnimationFrames);
mandelbrotZoomRatio = (mandelbrotEndSpan / mandelbrotStartSpan) ^ ...
  (1 / (mandelbrotFrameCount - 1));
mandelbrotColormap = mandelbrotPalette(mandelbrotMaxIterations);

mandelbrotSpan = mandelbrotStartSpan;
[mandelbrotImageData, mandelbrotRealAxis, mandelbrotImagAxis] = ...
  mandelbrotFrame(mandelbrotCenter, mandelbrotSpan, mandelbrotResolution, ...
  mandelbrotMaxIterations, mandelbrotUseGpu, mandelbrotColormap);

mandelbrotFigure = figure('Name', 'Mandelbrot on the GPU', ...
  'NumberTitle', 'off', 'Color', [0.02 0.02 0.05], ...
  'Visible', mandelbrotFigureVisible, 'Position', [80 60 900 820]);
mandelbrotAxes = axes('Parent', mandelbrotFigure, ...
  'Position', [0.05 0.10 0.90 0.84], 'Color', [0.02 0.02 0.05]);
% Prevent accidental pan or zoom from disturbing the live zoom animation.
disableDefaultInteractivity(mandelbrotAxes);
mandelbrotImage = image(mandelbrotAxes, 'XData', ...
  [mandelbrotRealAxis(1) mandelbrotRealAxis(end)], 'YData', ...
  [mandelbrotImagAxis(1) mandelbrotImagAxis(end)], 'CData', ...
  mandelbrotImageData);
axis(mandelbrotAxes, 'xy');
set(mandelbrotAxes, 'DataAspectRatio', [1 1 1], 'XColor', [0.6 0.64 0.74], ...
  'YColor', [0.6 0.64 0.74]);
mandelbrotDeviceLabel = 'CPU';
if mandelbrotUseGpu
  mandelbrotDeviceLabel = 'GPU';
end
title(mandelbrotAxes, sprintf('Mandelbrot zoom (%s)', mandelbrotDeviceLabel), ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
mandelbrotSpeedText = uicontrol('Parent', mandelbrotFigure, 'Style', 'text', ...
  'String', 'warming up...', 'Position', [20 12 560 22], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.02 0.02 0.05], ...
  'ForegroundColor', [0.80 0.86 0.96], 'FontSize', 10);

% A deep cosmic drone plays under the dive, looped by hand while zooming.
[mandelbrotPlayer, mandelbrotAudioDuration] = ...
  mandelbrotStartAudio(mandelbrotSoundEnabled);
mandelbrotAudioClock = tic();

if mandelbrotAnimationEnabled
  while isgraphics(mandelbrotFigure)
    mandelbrotSpan = mandelbrotStartSpan;
    for mandelbrotFrameIndex = 1:mandelbrotFrameCount
      if ~isgraphics(mandelbrotFigure)
        break
      end
      if ~isempty(mandelbrotPlayer) && ...
          toc(mandelbrotAudioClock) > mandelbrotAudioDuration
        [mandelbrotPlayer, mandelbrotAudioDuration] = ...
          mandelbrotStartAudio(mandelbrotSoundEnabled);
        mandelbrotAudioClock = tic();
      end
      try
        mandelbrotClock = tic();
        [mandelbrotImageData, mandelbrotRealAxis, mandelbrotImagAxis] = ...
          mandelbrotFrame(mandelbrotCenter, mandelbrotSpan, ...
          mandelbrotResolution, mandelbrotMaxIterations, ...
          mandelbrotUseGpu, mandelbrotColormap);
        set(mandelbrotImage, 'XData', ...
          [mandelbrotRealAxis(1) mandelbrotRealAxis(end)], 'YData', ...
          [mandelbrotImagAxis(1) mandelbrotImagAxis(end)], 'CData', ...
          mandelbrotImageData);
        set(mandelbrotAxes, 'XLim', ...
          [mandelbrotRealAxis(1) mandelbrotRealAxis(end)], 'YLim', ...
          [mandelbrotImagAxis(1) mandelbrotImagAxis(end)]);
        mandelbrotElapsed = toc(mandelbrotClock);
        mandelbrotPixels = mandelbrotResolution ^ 2;
        if isgraphics(mandelbrotSpeedText) && mandelbrotElapsed > 0
          set(mandelbrotSpeedText, 'String', sprintf(...
            '%s | span %.2g | %.0f k pixels in %.0f ms | %.0f Mpix/s', ...
            mandelbrotDeviceLabel, mandelbrotSpan, mandelbrotPixels / 1e3, ...
            1e3 * mandelbrotElapsed, ...
            mandelbrotPixels / mandelbrotElapsed / 1e6));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if mandelbrotFramePause > 0
        pause(mandelbrotFramePause);
      end
      mandelbrotSpan = mandelbrotSpan * mandelbrotZoomRatio;
    end
    if ~mandelbrotAnimationContinuous || ~isgraphics(mandelbrotFigure)
      break
    end
  end
end
mandelbrotStopAudio(mandelbrotPlayer);
%=============================================================================
function [player, duration] = mandelbrotStartAudio(enabled)
  player = [];
  duration = 1;
  if ~enabled || exist('audioplayer', 'builtin') == 0 || ...
      exist('audiodevinfo', 'builtin') == 0
    return
  end
  try
    devices = audiodevinfo();
    if isempty(devices.output)
      return
    end
    rate = 22050;
    duration = 12;
    time = (0:round(duration * rate) - 1) / rate;
    % A deep cosmic drone with a slow sweep, for the dive into the set.
    partials = 73.42 * [1 2 3 4.5];
    sweep = 0.6 + 0.4 * sin(2 * pi * 0.08 * time);
    drone = zeros(1, numel(time));
    for partialIndex = 1:numel(partials)
      weight = 1 / partialIndex;
      drone = drone + weight * sin(2 * pi * partials(partialIndex) * time);
    end
    swell = 0.5 - 0.5 * cos(2 * pi * time / duration);
    sample = 0.16 * (swell .* sweep .* drone) / numel(partials);
    player = audioplayer(sample(:), rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function mandelbrotStopAudio(player)
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
function [imageData, realAxis, imagAxis] = mandelbrotFrame(center, span, ...
  resolution, maxIterations, useGpu, colormap)
  realAxis = real(center) + linspace(-span / 2, span / 2, resolution);
  imagAxis = imag(center) + linspace(-span / 2, span / 2, resolution);
  [realGrid, imagGrid] = meshgrid(realAxis, imagAxis);
  c = complex(realGrid, imagGrid);
  if useGpu
    c = gpuArray(c);
  end
  z = c;
  escapeCount = zeros(resolution, resolution);
  if useGpu
    escapeCount = gpuArray(escapeCount);
  end
  % Fully vectorised escape-time iteration over the whole grid.
  for iteration = 1:maxIterations
    z = z .* z + c;
    escapeCount = escapeCount + (abs(z) <= 2);
  end
  if useGpu
    escapeCount = gather(escapeCount);
  end
  % Points that never escaped belong to the set and are drawn black.
  indices = min(maxIterations, max(1, round(escapeCount) + 1));
  imageData = mandelbrotApplyColormap(indices, colormap);
end
%=============================================================================
function colormap = mandelbrotPalette(levels)
  % A smooth deep-blue to gold band, with black reserved for the set interior.
  t = linspace(0, 1, levels).';
  red = 0.5 + 0.5 * sin(3.0 * t + 0.2);
  green = 0.5 + 0.5 * sin(3.0 * t + 1.6);
  blue = 0.5 + 0.5 * sin(3.0 * t + 3.4);
  colormap = [red green blue];
  colormap(end, :) = [0 0 0];
end
%=============================================================================
function imageData = mandelbrotApplyColormap(indices, colormap)
  rows = size(indices, 1);
  cols = size(indices, 2);
  flat = indices(:);
  imageData = zeros(rows, cols, 3);
  for channel = 1:3
    plane = colormap(flat, channel);
    imageData(:, :, channel) = reshape(plane, rows, cols);
  end
end
%=============================================================================
