%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compile a C Mandelbrot kernel as a MEX and zoom with it in real time.
if ~exist('mexMandelFigureVisible', 'var')
  mexMandelFigureVisible = 'on';
end
if ~exist('mexMandelAnimationEnabled', 'var')
  mexMandelAnimationEnabled = true;
end
if ~exist('mexMandelAnimationFrames', 'var')
  mexMandelAnimationFrames = 220;
end
if ~exist('mexMandelFramePause', 'var')
  mexMandelFramePause = 0;
end
if ~exist('mexMandelAnimationContinuous', 'var')
  mexMandelAnimationContinuous = strcmp(mexMandelFigureVisible, 'on');
end
if ~exist('mexMandelResolution', 'var')
  mexMandelResolution = 560;
end
if ~exist('mexMandelSoundEnabled', 'var')
  mexMandelSoundEnabled = strcmp(mexMandelFigureVisible, 'on');
end

% A C compiler is required to build the kernel; stop clearly without one.
if ~havecompiler()
  error('Nelson:mex_mandelbrot:noCompiler', ...
    'This demo needs a configured C compiler (see doc mex).');
end

mexMandelFigure = figure('Name', 'MEX Mandelbrot', 'NumberTitle', 'off', ...
  'Color', [0.02 0.02 0.05], 'Visible', mexMandelFigureVisible, ...
  'Position', [90 60 860 840]);
mexMandelAxes = axes('Parent', mexMandelFigure, ...
  'Position', [0.05 0.10 0.90 0.84], 'Color', [0.02 0.02 0.05]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(mexMandelAxes);

% Build the MEX kernel once, in a scratch directory added to the path.
mexMandelBuildDir = tempname();
mkdir(mexMandelBuildDir);
mexMandelSource = [modulepath('mex', 'root'), ...
  '/examples/mandelbrot_mex_kernel.c'];
copyfile(mexMandelSource, mexMandelBuildDir);
mexMandelPreviousDir = pwd();
mexMandelCleanup = onCleanup(@() cd(mexMandelPreviousDir));
cd(mexMandelBuildDir);
mex('mandelbrot_mex_kernel.c');
cd(mexMandelPreviousDir);
addpath(mexMandelBuildDir);

mexMandelMaxIterations = 200;
mexMandelCenter = complex(-0.743643887037151, 0.131825904205330);
mexMandelStartSpan = 3.2;
mexMandelEndSpan = 0.00025;
mexMandelFrameCount = max(2, mexMandelAnimationFrames);
mexMandelZoomRatio = (mexMandelEndSpan / mexMandelStartSpan) ^ ...
  (1 / (mexMandelFrameCount - 1));
mexMandelSpan = mexMandelStartSpan;
mexMandelCounts = mandelbrot_mex_kernel(real(mexMandelCenter), ...
  imag(mexMandelCenter), mexMandelSpan, mexMandelResolution, ...
  mexMandelMaxIterations);
mexMandelImage = image(mexMandelAxes, 'CData', ...
  mexMandelColorize(mexMandelCounts, mexMandelMaxIterations));
axis(mexMandelAxes, 'image');
axis(mexMandelAxes, 'off');
title(mexMandelAxes, 'Mandelbrot zoom from a compiled C MEX kernel', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
mexMandelSpeedText = uicontrol('Parent', mexMandelFigure, 'Style', 'text', ...
  'String', 'warming up...', 'Position', [20 12 580 22], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.02 0.02 0.05], ...
  'ForegroundColor', [0.80 0.86 0.96], 'FontSize', 10);

% A dark drone plays under the descent, looped by hand while zooming.
[mexMandelPlayer, mexMandelAudioDuration] = ...
  mexMandelStartAudio(mexMandelSoundEnabled);
mexMandelAudioClock = tic();

if mexMandelAnimationEnabled
  while isgraphics(mexMandelFigure)
    mexMandelSpan = mexMandelStartSpan;
    for mexMandelFrameIndex = 1:mexMandelFrameCount
      if ~isgraphics(mexMandelFigure)
        break
      end
      if ~isempty(mexMandelPlayer) && ...
          toc(mexMandelAudioClock) > mexMandelAudioDuration
        [mexMandelPlayer, mexMandelAudioDuration] = ...
          mexMandelStartAudio(mexMandelSoundEnabled);
        mexMandelAudioClock = tic();
      end
      try
        % Time only the compiled kernel so the read-out shows C throughput.
        mexMandelClock = tic();
        mexMandelCounts = mandelbrot_mex_kernel(real(mexMandelCenter), ...
          imag(mexMandelCenter), mexMandelSpan, mexMandelResolution, ...
          mexMandelMaxIterations);
        mexMandelElapsed = toc(mexMandelClock);
        set(mexMandelImage, 'CData', mexMandelColorize(mexMandelCounts, ...
          mexMandelMaxIterations));
        mexMandelPixels = mexMandelResolution ^ 2;
        if isgraphics(mexMandelSpeedText) && mexMandelElapsed > 0
          set(mexMandelSpeedText, 'String', sprintf(...
            'C MEX kernel | span %.2g | %.0f k pixels in %.1f ms | %.0f Mpix/s', ...
            mexMandelSpan, mexMandelPixels / 1e3, ...
            1e3 * mexMandelElapsed, ...
            mexMandelPixels / mexMandelElapsed / 1e6));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if mexMandelFramePause > 0
        pause(mexMandelFramePause);
      end
      mexMandelSpan = mexMandelSpan * mexMandelZoomRatio;
    end
    if ~mexMandelAnimationContinuous || ~isgraphics(mexMandelFigure)
      break
    end
  end
end
mexMandelStopAudio(mexMandelPlayer);
%=============================================================================
function [player, duration] = mexMandelStartAudio(enabled)
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
    % A low drone with a slow sub-octave beat, for the dive into the set.
    partials = 55 * [1 1.5 2 3];
    tremolo = 0.75 + 0.25 * sin(2 * pi * 0.3 * time);
    drone = zeros(1, numel(time));
    for partial = partials
      drone = drone + sin(2 * pi * partial * time);
    end
    swell = 0.5 - 0.5 * cos(2 * pi * time / duration);
    sample = 0.18 * (swell .* tremolo .* drone) / numel(partials);
    player = audioplayer(sample(:), rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function mexMandelStopAudio(player)
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
function imageData = mexMandelColorize(counts, maxIter)
  % Vectorised colouring straight from the escape counts, no table lookup.
  stretched = log(1 + counts) / log(1 + maxIter);
  rows = size(counts, 1);
  cols = size(counts, 2);
  imageData = zeros(rows, cols, 3);
  imageData(:, :, 1) = 0.5 + 0.5 * sin(6.2 * stretched + 0.2);
  imageData(:, :, 2) = 0.5 + 0.5 * sin(6.2 * stretched + 2.2);
  imageData(:, :, 3) = 0.5 + 0.5 * sin(6.2 * stretched + 4.0);
  inSet = counts >= maxIter - 0.5;
  for channel = 1:3
    plane = imageData(:, :, channel);
    plane(inSet) = 0;
    imageData(:, :, channel) = plane;
  end
end
%=============================================================================
