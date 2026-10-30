%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animated Newton basins of a polynomial, zooming into the fractal boundary.
if ~exist('newtonFractalFigureVisible', 'var')
  newtonFractalFigureVisible = 'on';
end
if ~exist('newtonFractalAnimationEnabled', 'var')
  newtonFractalAnimationEnabled = true;
end
if ~exist('newtonFractalAnimationFrames', 'var')
  newtonFractalAnimationFrames = 220;
end
if ~exist('newtonFractalFramePause', 'var')
  newtonFractalFramePause = 0;
end
if ~exist('newtonFractalAnimationContinuous', 'var')
  newtonFractalAnimationContinuous = ...
    strcmp(newtonFractalFigureVisible, 'on');
end
% Grid resolution per side; tests shrink it so batch runs stay fast.
if ~exist('newtonFractalResolution', 'var')
  if strcmp(newtonFractalFigureVisible, 'on')
    newtonFractalResolution = 220;
  else
    newtonFractalResolution = 120;
  end
end
if ~exist('newtonFractalSoundEnabled', 'var')
  newtonFractalSoundEnabled = strcmp(newtonFractalFigureVisible, 'on');
end

% Polynomial z^3 - 1: its three roots are the cube roots of unity, which give
% the classic three-coloured Newton basins.
newtonFractalCoefficients = [1 0 0 -1];
newtonFractalDerivative = polyder(newtonFractalCoefficients);
newtonFractalRoots = roots(newtonFractalCoefficients);
newtonFractalRootColors = [0.95 0.35 0.32; ...
  0.30 0.72 0.46; 0.26 0.52 0.96];
newtonFractalMaxIterations = 20;
newtonFractalTolerance = 1e-5;

% Zoom from the whole picture down to a rich point on a basin boundary.
if ~exist('newtonFractalCenter', 'var')
  newtonFractalCenter = complex(-0.5, 0.0);
end
if ~exist('newtonFractalStartSpan', 'var')
  newtonFractalStartSpan = 3.0;
end
if ~exist('newtonFractalEndSpan', 'var')
  newtonFractalEndSpan = 0.012;
end
newtonFractalFrameCount = max(2, newtonFractalAnimationFrames);
newtonFractalZoomRatio = ...
  (newtonFractalEndSpan / newtonFractalStartSpan) ^ ...
  (1 / (newtonFractalFrameCount - 1));

newtonFractalFigure = figure('Name', 'Newton fractal', ...
  'NumberTitle', 'off', 'Color', [0.05 0.06 0.09], ...
  'Visible', newtonFractalFigureVisible, 'Position', [80 80 900 760]);
newtonFractalAxes = axes('Parent', newtonFractalFigure, ...
  'Position', [0.06 0.11 0.88 0.82], 'Color', [0.05 0.06 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(newtonFractalAxes);
newtonFractalSpan = newtonFractalStartSpan;
[newtonFractalImageData, newtonFractalRealAxis, newtonFractalImagAxis] = ...
  newtonFractalFrame(newtonFractalCenter, newtonFractalSpan, ...
  newtonFractalResolution, newtonFractalCoefficients, ...
  newtonFractalDerivative, newtonFractalRoots, ...
  newtonFractalRootColors, newtonFractalMaxIterations, ...
  newtonFractalTolerance);
newtonFractalImage = image(newtonFractalAxes, ...
  'XData', newtonFractalRealAxis, 'YData', newtonFractalImagAxis, ...
  'CData', newtonFractalImageData);
axis(newtonFractalAxes, 'xy');
set(newtonFractalAxes, 'DataAspectRatio', [1 1 1], ...
  'XLim', [newtonFractalRealAxis(1) newtonFractalRealAxis(end)], ...
  'YLim', [newtonFractalImagAxis(1) newtonFractalImagAxis(end)], ...
  'XColor', [0.70 0.74 0.82], 'YColor', [0.70 0.74 0.82]);
xlabel(newtonFractalAxes, 'Real axis');
ylabel(newtonFractalAxes, 'Imaginary axis');
title(newtonFractalAxes, 'Newton basins of z^3 - 1', ...
  'Color', [0.92 0.94 0.98]);

% A live speed read-out turns the zoom into a benchmark of the engine.
newtonFractalSpeedText = uicontrol('Parent', newtonFractalFigure, ...
  'Style', 'text', 'String', 'warming up...', ...
  'Position', [20 14 520 22], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.09], ...
  'ForegroundColor', [0.80 0.86 0.96], 'FontSize', 10);

% An ambient triad plays under the zoom, looped by hand while animating.
[newtonFractalPlayer, newtonFractalAudioDuration] = ...
  newtonFractalStartAudio(newtonFractalSoundEnabled);
newtonFractalAudioClock = tic();

if newtonFractalAnimationEnabled
  while isgraphics(newtonFractalFigure)
    newtonFractalSpan = newtonFractalStartSpan;
    for newtonFractalFrameIndex = 1:newtonFractalFrameCount
      if ~isgraphics(newtonFractalFigure)
        break
      end
      if ~isempty(newtonFractalPlayer) && ...
          toc(newtonFractalAudioClock) > newtonFractalAudioDuration
        [newtonFractalPlayer, newtonFractalAudioDuration] = ...
          newtonFractalStartAudio(newtonFractalSoundEnabled);
        newtonFractalAudioClock = tic();
      end
      try
        newtonFractalClock = tic();
        [newtonFractalImageData, newtonFractalRealAxis, ...
          newtonFractalImagAxis] = newtonFractalFrame(...
          newtonFractalCenter, newtonFractalSpan, ...
          newtonFractalResolution, newtonFractalCoefficients, ...
          newtonFractalDerivative, newtonFractalRoots, ...
          newtonFractalRootColors, newtonFractalMaxIterations, ...
          newtonFractalTolerance);
        set(newtonFractalImage, 'XData', newtonFractalRealAxis, ...
          'YData', newtonFractalImagAxis, 'CData', newtonFractalImageData);
        % Follow the zoom so the fractal always fills the view.
        set(newtonFractalAxes, ...
          'XLim', [newtonFractalRealAxis(1) newtonFractalRealAxis(end)], ...
          'YLim', [newtonFractalImagAxis(1) newtonFractalImagAxis(end)]);
        newtonFractalElapsed = toc(newtonFractalClock);
        newtonFractalPixels = newtonFractalResolution ^ 2;
        if newtonFractalElapsed > 0
          set(newtonFractalSpeedText, 'String', sprintf(...
            'span %.3g | %.0f k pixels in %.0f ms | %.1f Mpix/s', ...
            newtonFractalSpan, newtonFractalPixels / 1e3, ...
            1e3 * newtonFractalElapsed, ...
            newtonFractalPixels / newtonFractalElapsed / 1e6));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if newtonFractalFramePause > 0
        pause(newtonFractalFramePause);
      end
      newtonFractalSpan = newtonFractalSpan * newtonFractalZoomRatio;
    end
    if ~newtonFractalAnimationContinuous || ~isgraphics(newtonFractalFigure)
      break
    end
  end
end
newtonFractalStopAudio(newtonFractalPlayer);
%=============================================================================
function [player, duration] = newtonFractalStartAudio(enabled)
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
    duration = 10;
    time = (0:round(duration * rate) - 1) / rate;
    % A bright triad for the three basins, each voice gently pulsing.
    notes = [220.00 277.18 329.63];
    pad = zeros(1, numel(time));
    for noteIndex = 1:numel(notes)
      phase = 2 * pi * notes(noteIndex) * time;
      gate = 0.5 + 0.5 * sin(2 * pi * 0.25 * time - noteIndex);
      pad = pad + gate .* (sin(phase) + 0.3 * sin(2 * phase));
    end
    swell = 0.5 - 0.5 * cos(2 * pi * time / duration);
    sample = 0.14 * swell .* pad / numel(notes);
    player = audioplayer(sample(:), rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function newtonFractalStopAudio(player)
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
function [imageData, realAxis, imagAxis] = newtonFractalFrame(center, ...
  span, resolution, coefficients, derivative, rootList, rootColors, ...
  maxIterations, tolerance)
  % Build the complex grid for this zoom window.
  realAxis = real(center) + linspace(-span / 2, span / 2, resolution);
  imagAxis = imag(center) + linspace(-span / 2, span / 2, resolution);
  [realGrid, imagGrid] = meshgrid(realAxis, imagAxis);
  z = complex(realGrid, imagGrid);
  iterationMap = zeros(resolution, resolution);
  % Fully vectorised Newton iteration over every pixel at once; staying
  % mask-free (no per-pixel logical indexing) is what keeps the frame fast.
  for iteration = 1:maxIterations
    values = polyval(coefficients, z);
    slopes = polyval(derivative, z);
    z = z - values ./ (slopes + 1e-30);
    % Count the iterations each pixel still needed, for the convergence shading.
    iterationMap = iterationMap + (abs(values) > tolerance);
  end
  % Classify each pixel by the root it landed on.
  rootCount = numel(rootList);
  distances = zeros(resolution, resolution, rootCount);
  for rootIndex = 1:rootCount
    distances(:, :, rootIndex) = abs(z - rootList(rootIndex));
  end
  [~, nearestRoot] = min(distances, [], 3);
  % Shade by how fast the pixel converged: quick points stay bright.
  shade = 1 - min(iterationMap, maxIterations) / maxIterations;
  shade = 0.25 + 0.75 * shade;
  imageData = zeros(resolution, resolution, 3);
  for rootIndex = 1:rootCount
    mask = nearestRoot == rootIndex;
    for channel = 1:3
      plane = imageData(:, :, channel);
      plane(mask) = rootColors(rootIndex, channel) * shade(mask);
      imageData(:, :, channel) = plane;
    end
  end
end
%=============================================================================
