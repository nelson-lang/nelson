%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate the standing-wave normal modes of a vibrating rectangular membrane.
if ~exist('membraneFigureVisible', 'var')
  membraneFigureVisible = 'on';
end
if ~exist('membraneBatch', 'var')
  membraneBatch = false;
end
if ~exist('membraneMaxFrames', 'var')
  membraneMaxFrames = 300;
end
if ~exist('membraneFramePause', 'var')
  membraneFramePause = 0.006;
end
if ~exist('membraneOutputImage', 'var')
  membraneOutputImage = '';
end

% Spatial grid on the unit square [0, 1] x [0, 1].
% The whole mesh is recomputed and repainted every frame, so the per-frame
% cost scales with the number of vertices; 42 keeps the lit surface smooth
% and pretty while trimming the count from the original grid.
gridSize = 42;
xVector = linspace(0, 1, gridSize);
yVector = linspace(0, 1, gridSize);
[gridX, gridY] = meshgrid(xVector, yVector);

% Normal modes of a rectangular membrane clamped on all edges.
% u(x, y, t) = sum A_mn sin(m pi x) sin(n pi y) cos(w_mn t),
% with angular frequency w_mn = pi sqrt(m^2 + n^2).
modeM = [1 1 2 2 1];
modeN = [1 2 1 2 3];
modeCount = numel(modeM);
modeFrequency = pi * sqrt(modeM .^ 2 + modeN .^ 2);

% Precompute the spatial shape of every mode once on the grid.
modeShapes = cell(1, modeCount);
for modeIndex = 1:modeCount
  modeShapes{modeIndex} = ...
    sin(modeM(modeIndex) * pi * gridX) .* sin(modeN(modeIndex) * pi * gridY);
end

% Default amplitudes; the three first are steered live by the sliders.
defaultAmplitudes = [0.65 0.45 0.35 0.20 0.25];
amplitudes = defaultAmplitudes;

verticalLimit = 1.6;
initialZ = membraneModeSum(modeShapes, amplitudes, modeFrequency, 0);

figureHandle = figure('Name', 'Vibrating membrane - normal modes', ...
  'NumberTitle', 'off', 'Color', [0.08 0.09 0.12], ...
  'Visible', membraneFigureVisible, 'Position', [80 80 960 720]);
axesHandle = axes('Parent', figureHandle, 'Position', [0.08 0.20 0.86 0.74]);
surfaceHandle = surf(axesHandle, gridX, gridY, initialZ, initialZ, ...
  'EdgeColor', 'none', 'FaceLighting', 'gouraud');
colormap(axesHandle, parula(256));
axis(axesHandle, [0 1 0 1 -verticalLimit verticalLimit]);
zlim(axesHandle, [-verticalLimit verticalLimit]);
caxis(axesHandle, [-verticalLimit verticalLimit]);
view(axesHandle, [-37.5 32]);
set(axesHandle, 'Color', [0.08 0.09 0.12], 'XColor', [0.75 0.78 0.85], ...
  'YColor', [0.75 0.78 0.85], 'ZColor', [0.75 0.78 0.85], ...
  'GridColor', [0.35 0.38 0.45]);
grid(axesHandle, 'on');
xlabel(axesHandle, 'x');
ylabel(axesHandle, 'y');
zlabel(axesHandle, 'displacement');
title(axesHandle, 'Vibrating membrane - normal modes', ...
  'Color', [0.90 0.92 0.96]);
light('Parent', axesHandle, 'Position', [-1 -1 2], 'Style', 'infinite');

% Interactive controls: three mode-amplitude sliders and three buttons.
sliderLabels = {'Mode (1,1)', 'Mode (1,2)', 'Mode (2,1)'};
sliderHandles = cell(1, 3);
for sliderIndex = 1:3
  labelLeft = 40 + (sliderIndex - 1) * 300;
  uicontrol('Parent', figureHandle, 'Style', 'text', ...
    'String', sliderLabels{sliderIndex}, 'Position', [labelLeft 96 120 20], ...
    'HorizontalAlignment', 'left', 'BackgroundColor', [0.08 0.09 0.12], ...
    'ForegroundColor', [0.85 0.88 0.94]);
  sliderHandles{sliderIndex} = uicontrol('Parent', figureHandle, ...
    'Style', 'slider', 'Min', 0, 'Max', 1, ...
    'Value', defaultAmplitudes(sliderIndex), ...
    'Position', [labelLeft 72 220 22]);
end

startButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Start', 'Position', [40 24 120 30], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
stopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Stop', 'Position', [180 24 120 30], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
resetButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Reset', 'Position', [320 24 120 30], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));

if membraneBatch
  % Deterministic single pass for batch validation.
  frameLimit = min(membraneMaxFrames, 60);
  simulationTime = 0;
  timeStep = 0.05;
  for frameIndex = 1:frameLimit
    if ~isgraphics(figureHandle) || ~isgraphics(surfaceHandle)
      break
    end
    simulationTime = simulationTime + timeStep;
    currentZ = membraneModeSum(modeShapes, amplitudes, ...
      modeFrequency, simulationTime);
    set(surfaceHandle, 'ZData', currentZ, 'CData', currentZ);
    drawnow();
  end
  if ~isempty(membraneOutputImage) && isgraphics(figureHandle)
    drawnow();
    saveas(figureHandle, membraneOutputImage);
  end
else
  % Interactive loop: poll sliders and buttons live.
  running = true;
  simulationTime = 0;
  timeStep = 0.04;
  % Keep polling the controls for the lifetime of the figure so Start / Stop /
  % Reset stay live (the frame cap is only used by the batch test path).
  while isgraphics(figureHandle)
    if isgraphics(startButton) && startButton.UserData
      set(startButton, 'UserData', false);
      running = true;
    end
    if isgraphics(stopButton) && stopButton.UserData
      set(stopButton, 'UserData', false);
      running = false;
    end
    if isgraphics(resetButton) && resetButton.UserData
      set(resetButton, 'UserData', false);
      simulationTime = 0;
      for sliderIndex = 1:3
        if isgraphics(sliderHandles{sliderIndex})
          set(sliderHandles{sliderIndex}, ...
            'Value', defaultAmplitudes(sliderIndex));
        end
      end
    end
    for sliderIndex = 1:3
      if isgraphics(sliderHandles{sliderIndex})
        amplitudes(sliderIndex) = sliderHandles{sliderIndex}.Value;
      end
    end
    if running
      simulationTime = simulationTime + timeStep;
    end
    if ~isgraphics(surfaceHandle)
      break
    end
    currentZ = membraneModeSum(modeShapes, amplitudes, ...
      modeFrequency, simulationTime);
    set(surfaceHandle, 'ZData', currentZ, 'CData', currentZ);
    drawnow();
    pause(membraneFramePause);
  end
end
%=============================================================================
function totalField = membraneModeSum(modeShapes, amplitudes, ...
  modeFrequency, currentTime)
  totalField = zeros(size(modeShapes{1}));
  for modeIndex = 1:numel(modeShapes)
    totalField = totalField + amplitudes(modeIndex) * ...
      modeShapes{modeIndex} * cos(modeFrequency(modeIndex) * currentTime);
  end
end
%=============================================================================
