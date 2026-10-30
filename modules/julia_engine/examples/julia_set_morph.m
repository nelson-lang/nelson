%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compute a Julia set in the Julia language each frame and morph its constant.
if ~exist('juliaFigureVisible', 'var')
  juliaFigureVisible = 'on';
end
if ~exist('juliaAnimationEnabled', 'var')
  juliaAnimationEnabled = true;
end
if ~exist('juliaAnimationFrames', 'var')
  juliaAnimationFrames = 400;
end
if ~exist('juliaFramePause', 'var')
  juliaFramePause = 0;
end
if ~exist('juliaAnimationContinuous', 'var')
  juliaAnimationContinuous = strcmp(juliaFigureVisible, 'on');
end
if ~exist('juliaResolution', 'var')
  juliaResolution = 420;
end
if ~exist('juliaSoundEnabled', 'var')
  juliaSoundEnabled = strcmp(juliaFigureVisible, 'on');
end

% A Julia engine must be reachable; stop clearly when it is not.
juliaReady = false;
try
  jlrun('1 + 1');
  juliaReady = true;
catch
end
if ~juliaReady
  error('Nelson:julia_set_morph:noJulia', ...
    'This demo needs a configured Julia engine (see doc jlrun).');
end

newlineChar = char(10);
% Define the escape-time Julia-set kernel once; Julia JIT-compiles it once.
juliaKernel = strjoin({ ...
  'function juliaset(cr, ci, N)', ...
  '  g = zeros(N, N)', ...
  '  for i in 1:N', ...
  '    for j in 1:N', ...
  '      x = (j - 1) / (N - 1) * 3.2 - 1.6', ...
  '      y = (i - 1) / (N - 1) * 3.2 - 1.6', ...
  '      zr = x; zi = y; k = 0', ...
  '      while k < 150 && zr * zr + zi * zi < 4.0', ...
  '        zrn = zr * zr - zi * zi + cr', ...
  '        zi = 2.0 * zr * zi + ci', ...
  '        zr = zrn', ...
  '        k += 1', ...
  '      end', ...
  '      g[i, j] = k', ...
  '    end', ...
  '  end', ...
  '  return g', ...
  'end'}, newlineChar);
jlrun(juliaKernel);
juliaColormap = juliaPalette(151);

juliaConstant = 0.7885 * [cos(0) sin(0)];
juliaMaxIterations = 150;
juliaGrid = double(jlrun(sprintf('g = juliaset(%.6f, %.6f, %d)', ...
  juliaConstant(1), juliaConstant(2), juliaResolution), 'g'));

juliaFigure = figure('Name', 'Julia set in Julia', 'NumberTitle', 'off', ...
  'Color', [0.02 0.02 0.05], 'Visible', juliaFigureVisible, ...
  'Position', [100 70 820 820]);
juliaAxes = axes('Parent', juliaFigure, 'Position', [0.04 0.08 0.92 0.86]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(juliaAxes);
juliaImage = image(juliaAxes, 'CData', ...
  juliaApplyColormap(juliaGrid, juliaColormap, juliaMaxIterations));
axis(juliaAxes, 'image');
axis(juliaAxes, 'off');
title(juliaAxes, 'Julia set computed in the Julia language', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
juliaInfo = uicontrol('Parent', juliaFigure, 'Style', 'text', 'String', ...
  '', 'Position', [16 8 720 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.02 0.02 0.05], 'ForegroundColor', [0.80 0.86 0.96], ...
  'FontSize', 10);

% An ambient synthwave pad plays under the morphing fractal, looped by hand.
[juliaPlayer, juliaAudioDuration] = juliaStartAudio(juliaSoundEnabled);
juliaAudioClock = tic();

if juliaAnimationEnabled
  juliaFrame = 0;
  while isgraphics(juliaFigure)
    juliaFrame = juliaFrame + 1;
    if juliaFrame > juliaAnimationFrames && ~juliaAnimationContinuous
      break
    end
    if ~isempty(juliaPlayer) && toc(juliaAudioClock) > juliaAudioDuration
      [juliaPlayer, juliaAudioDuration] = juliaStartAudio(juliaSoundEnabled);
      juliaAudioClock = tic();
    end
    if ~isgraphics(juliaFigure)
      break
    end
    try
      theta = juliaFrame * 0.025;
      juliaConstant = 0.7885 * [cos(theta) sin(theta)];
      juliaClock = tic();
      juliaGrid = double(jlrun(sprintf('g = juliaset(%.6f, %.6f, %d)', ...
        juliaConstant(1), juliaConstant(2), juliaResolution), 'g'));
      set(juliaImage, 'CData', juliaApplyColormap(juliaGrid, ...
        juliaColormap, juliaMaxIterations));
      juliaElapsed = toc(juliaClock);
      if isgraphics(juliaInfo) && juliaElapsed > 0
        set(juliaInfo, 'String', sprintf(...
          'c = %.3f + %.3fi | %dx%d grid in %.0f ms in Julia', ...
          juliaConstant(1), juliaConstant(2), juliaResolution, ...
          juliaResolution, 1e3 * juliaElapsed));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if juliaFramePause > 0
      pause(juliaFramePause);
    end
  end
end
juliaStopAudio(juliaPlayer);
%=============================================================================
function [player, duration] = juliaStartAudio(enabled)
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
    duration = 14;
    time = (0:round(duration * rate) - 1) / rate;
    % A slow detuned pad on a minor-ish stack of partials.
    partials = 110 * [1 1.5 1.78 2.0];
    vibrato = 1 + 0.004 * sin(2 * pi * 5 * time);
    pad = zeros(1, numel(time));
    for partial = partials
      pad = pad + sin(2 * pi * partial * time .* vibrato);
    end
    swell = 0.5 - 0.5 * cos(2 * pi * time / duration);
    sample = 0.16 * (swell .* pad) / numel(partials);
    player = audioplayer(sample(:), rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function juliaStopAudio(player)
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
function colormap = juliaPalette(levels)
  t = linspace(0, 1, levels).';
  colormap = [0.5 + 0.5 * sin(6.2 * t + 0.0), ...
    0.5 + 0.5 * sin(6.2 * t + 2.1), 0.5 + 0.5 * sin(6.2 * t + 4.2)];
end
%=============================================================================
function imageData = juliaApplyColormap(grid, colormap, maxIter)
  levels = size(colormap, 1);
  % Log-stretch the escape counts so sparse "dust" sets still show colour.
  stretched = log(1 + grid) / log(1 + maxIter);
  indices = min(levels, max(1, round(stretched * (levels - 1)) + 1));
  rows = size(grid, 1);
  cols = size(grid, 2);
  flat = indices(:);
  imageData = zeros(rows, cols, 3);
  for channel = 1:3
    plane = colormap(flat, channel);
    imageData(:, :, channel) = reshape(plane, rows, cols);
  end
  % Points that never escaped belong to the set and are drawn black.
  inSet = grid >= maxIter - 0.5;
  for channel = 1:3
    plane = imageData(:, :, channel);
    plane(inSet) = 0;
    imageData(:, :, channel) = plane;
  end
end
%=============================================================================
