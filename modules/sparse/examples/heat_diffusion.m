%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Diffuse heat on a plate with a sparse Laplacian while hot spots move around.
if ~exist('heatFigureVisible', 'var')
  heatFigureVisible = 'on';
end
if ~exist('heatAnimationEnabled', 'var')
  heatAnimationEnabled = true;
end
if ~exist('heatAnimationFrames', 'var')
  heatAnimationFrames = 900;
end
if ~exist('heatFramePause', 'var')
  heatFramePause = 0;
end
if ~exist('heatAnimationContinuous', 'var')
  heatAnimationContinuous = strcmp(heatFigureVisible, 'on');
end
if ~exist('heatGridSize', 'var')
  heatGridSize = 130;
end

heatSide = heatGridSize;
heatCellCount = heatSide * heatSide;
% Build the two-dimensional five-point Laplacian as a sparse matrix.
heatUnit = ones(heatSide, 1);
heatLine = spdiags([heatUnit -2 * heatUnit heatUnit], [-1 0 1], ...
  heatSide, heatSide);
heatIdentity = speye(heatSide);
heatLaplacian = kron(heatIdentity, heatLine) + ...
  kron(heatLine, heatIdentity);
heatField = zeros(heatCellCount, 1);
heatDt = 0.18;
heatSubSteps = 3;

heatFigure = figure('Name', 'Sparse heat diffusion', 'NumberTitle', 'off', ...
  'Color', [0.03 0.03 0.05], 'Visible', heatFigureVisible, ...
  'Position', [110 80 820 820]);
heatAxes = axes('Parent', heatFigure, 'Position', [0.04 0.08 0.92 0.86]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(heatAxes);
heatImage = image(heatAxes, 'CData', heatColorize(...
  reshape(heatField, heatSide, heatSide)));
axis(heatAxes, 'image');
axis(heatAxes, 'off');
heatTitle = title(heatAxes, 'Heat diffusion through a sparse Laplacian', ...
  'Color', [0.94 0.94 0.96], 'FontSize', 13);
heatInfoText = uicontrol('Parent', heatFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 10 780 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.03 0.03 0.05], 'ForegroundColor', [0.95 0.8 0.6], ...
  'FontSize', 10);

if heatAnimationEnabled
  heatFrame = 0;
  while isgraphics(heatFigure)
    heatFrame = heatFrame + 1;
    if heatFrame > heatAnimationFrames && ~heatAnimationContinuous
      break
    end
    if ~isgraphics(heatFigure)
      break
    end
    try
      heatClock = tic();
      % Two hot spots orbit the plate and keep injecting heat.
      angle = heatFrame * 0.05;
      sources = heatSourceIndices(heatSide, angle);
      heatField(sources) = 1.0;
      % Several explicit diffusion sub-steps spread the heat each frame.
      for subStep = 1:heatSubSteps
        heatField = heatField + heatDt * (heatLaplacian * heatField);
        heatField(sources) = 1.0;
      end
      set(heatImage, 'CData', heatColorize(...
        reshape(heatField, heatSide, heatSide)));
      heatElapsed = toc(heatClock);
      if isgraphics(heatInfoText) && heatElapsed > 0
        set(heatInfoText, 'String', sprintf(...
          '%d cells | sparse Laplacian nnz = %d | %.0f steps/s', ...
          heatCellCount, nnz(heatLaplacian), ...
          heatSubSteps / heatElapsed));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if heatFramePause > 0
      pause(heatFramePause);
    end
  end
end
%=============================================================================
function indices = heatSourceIndices(side, angle)
  % Two orbiting hot spots, returned as linear indices into the plate.
  radius = 0.30 * side;
  center = side / 2;
  rows = round(center + radius * [sin(angle) -sin(angle)]);
  cols = round(center + radius * [cos(angle) -cos(angle)]);
  rows = min(max(rows, 2), side - 1);
  cols = min(max(cols, 2), side - 1);
  indices = (cols - 1) * side + rows;
end
%=============================================================================
function rgb = heatColorize(field)
  % Map the temperature to a black-red-yellow-white hot gradient.
  value = min(max(field, 0), 1);
  red = min(1, value * 3);
  green = min(1, max(0, value * 3 - 1));
  blue = min(1, max(0, value * 3 - 2));
  rgb = zeros(size(field, 1), size(field, 2), 3);
  rgb(:, :, 1) = red;
  rgb(:, :, 2) = green;
  rgb(:, :, 3) = blue;
end
%=============================================================================
