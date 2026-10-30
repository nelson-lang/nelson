%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Step a 2-D wave equation in NumPy each frame and render it live in Nelson.
if ~exist('waveFigureVisible', 'var')
  waveFigureVisible = 'on';
end
if ~exist('waveAnimationEnabled', 'var')
  waveAnimationEnabled = true;
end
if ~exist('waveAnimationFrames', 'var')
  waveAnimationFrames = 900;
end
if ~exist('waveFramePause', 'var')
  waveFramePause = 0;
end
if ~exist('waveAnimationContinuous', 'var')
  waveAnimationContinuous = strcmp(waveFigureVisible, 'on');
end

% NumPy must be importable through the Python engine; stop clearly if not.
waveNumpyReady = false;
try
  pyrun('import numpy as np');
  waveNumpyReady = true;
catch
end
if ~waveNumpyReady
  error('Nelson:numpy_wave:noNumPy', ...
    'This demo needs a Python engine with NumPy (see doc pyrun).');
end

newlineChar = char(10);
% Set up the NumPy field once; the Python session keeps the state between calls.
waveInitCode = strjoin({ ...
  'import numpy as np', ...
  'N = 150', ...
  'u = np.zeros((N, N))', ...
  'vel = np.zeros((N, N))', ...
  'tick = 0'}, newlineChar);
pyrun(waveInitCode);
% One explicit wave step, fully vectorised in NumPy, driven by two sources.
waveStepCode = strjoin({ ...
  'tick += 1', ...
  'lap = (-4.0 * u + np.roll(u, 1, 0) + np.roll(u, -1, 0)', ...
  '       + np.roll(u, 1, 1) + np.roll(u, -1, 1))', ...
  'vel += 0.20 * lap', ...
  'vel *= 0.997', ...
  'u += vel', ...
  'u[45, 45] += 0.9 * np.sin(tick * 0.35)', ...
  'u[110, 95] += 0.7 * np.sin(tick * 0.52)', ...
  'frame = u.copy()'}, newlineChar);
waveField = double(pyrun(waveStepCode, 'frame'));
waveSize = size(waveField, 1);

waveFigure = figure('Name', 'NumPy wave', 'NumberTitle', 'off', ...
  'Color', [0.03 0.04 0.07], 'Visible', waveFigureVisible, ...
  'Position', [120 80 800 800]);
waveAxes = axes('Parent', waveFigure, 'Position', [0.04 0.06 0.92 0.88]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(waveAxes);
waveImage = image(waveAxes, 'CData', waveColor(waveField));
axis(waveAxes, 'image');
axis(waveAxes, 'off');
title(waveAxes, 'Wave equation stepped in NumPy, drawn in Nelson', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
waveInfo = uicontrol('Parent', waveFigure, 'Style', 'text', 'String', '', ...
  'Position', [16 8 720 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.03 0.04 0.07], 'ForegroundColor', [0.78 0.86 0.96], ...
  'FontSize', 10);

if waveAnimationEnabled
  waveFrame = 0;
  while isgraphics(waveFigure)
    waveFrame = waveFrame + 1;
    if waveFrame > waveAnimationFrames && ~waveAnimationContinuous
      break
    end
    if ~isgraphics(waveFigure)
      break
    end
    try
      waveClock = tic();
      % NumPy advances the whole field; Nelson only draws the result.
      waveField = double(pyrun(waveStepCode, 'frame'));
      set(waveImage, 'CData', waveColor(waveField));
      waveElapsed = toc(waveClock);
      if isgraphics(waveInfo) && waveElapsed > 0
        set(waveInfo, 'String', sprintf(...
          '%dx%d field stepped in NumPy | %.0f steps/s via pyrun', ...
          waveSize, waveSize, 1 / waveElapsed));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if waveFramePause > 0
      pause(waveFramePause);
    end
  end
end
%=============================================================================
function rgb = waveColor(field)
  % Map the signed wave height to a teal-black-magenta display.
  value = max(min(field / 1.3, 1), -1);
  positive = max(value, 0);
  negative = max(-value, 0);
  rows = size(field, 1);
  cols = size(field, 2);
  rgb = zeros(rows, cols, 3);
  rgb(:, :, 1) = 0.05 + 0.95 * positive;
  rgb(:, :, 2) = 0.05 + 0.75 * negative;
  rgb(:, :, 3) = 0.10 + 0.90 * negative + 0.30 * positive;
end
%=============================================================================
