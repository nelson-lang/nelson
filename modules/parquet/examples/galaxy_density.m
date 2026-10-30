%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Write a big star table to Parquet, stream it back and build a live density.
if ~exist('galaxyFigureVisible', 'var')
  galaxyFigureVisible = 'on';
end
if ~exist('galaxyAnimationEnabled', 'var')
  galaxyAnimationEnabled = true;
end
if ~exist('galaxyFramePause', 'var')
  galaxyFramePause = 0;
end
if ~exist('galaxyAnimationContinuous', 'var')
  galaxyAnimationContinuous = strcmp(galaxyFigureVisible, 'on');
end
if ~exist('galaxyStarCount', 'var')
  galaxyStarCount = 200000;
end
rng(2026);

% Generate a two-arm spiral galaxy and write every star to a Parquet file.
radius = sqrt(rand(galaxyStarCount, 1)) * 10;
arm = randi(2, galaxyStarCount, 1);
theta = radius * 0.55 + (arm - 1) * pi + randn(galaxyStarCount, 1) * 0.28;
starX = radius .* cos(theta) + randn(galaxyStarCount, 1) * 0.35;
starY = radius .* sin(theta) + randn(galaxyStarCount, 1) * 0.35;
galaxyFile = [tempname(), '.parquet'];
galaxyTable = table(starX, starY, 'VariableNames', {'x', 'y'});
parquetwrite(galaxyFile, galaxyTable);
% Read the whole catalogue back from Parquet.
galaxyLoaded = parquetread(galaxyFile);
galaxyX = galaxyLoaded.x;
galaxyY = galaxyLoaded.y;

galaxyBins = 240;
galaxyLimit = 12;
galaxyEdges = linspace(-galaxyLimit, galaxyLimit, galaxyBins + 1);
galaxyColumn = galaxyBinIndex(galaxyX, galaxyLimit, galaxyBins);
galaxyRow = galaxyBinIndex(galaxyY, galaxyLimit, galaxyBins);
galaxyDensity = zeros(galaxyBins, galaxyBins);
galaxyBatch = 5000;
galaxyProcessed = 0;

galaxyFigure = figure('Name', 'Parquet galaxy', 'NumberTitle', 'off', ...
  'Color', [0.01 0.01 0.03], 'Visible', galaxyFigureVisible, ...
  'Position', [120 70 800 800]);
galaxyAxes = axes('Parent', galaxyFigure, 'Position', [0.03 0.05 0.94 0.9], ...
  'Color', [0.01 0.01 0.03]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(galaxyAxes);
galaxyImage = image(galaxyAxes, 'CData', galaxyColor(galaxyDensity));
axis(galaxyAxes, 'image');
axis(galaxyAxes, 'off');
set(galaxyAxes, 'YDir', 'normal');
title(galaxyAxes, 'Star density streamed from a Parquet file', ...
  'Color', [0.90 0.92 0.98], 'FontSize', 13);
galaxyInfo = uicontrol('Parent', galaxyFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 720 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.01 0.01 0.03], 'ForegroundColor', [0.78 0.84 0.96], ...
  'FontSize', 10);

if galaxyAnimationEnabled
  while isgraphics(galaxyFigure)
    if ~isgraphics(galaxyFigure)
      break
    end
    try
      % Accumulate the next batch of stars into the 2-D density grid.
      first = galaxyProcessed + 1;
      last = min(galaxyProcessed + galaxyBatch, galaxyStarCount);
      if first <= last
        galaxyDensity = galaxyDensity + accumarray(...
          [galaxyRow(first:last) galaxyColumn(first:last)], 1, ...
          [galaxyBins galaxyBins]);
        galaxyProcessed = last;
      end
      set(galaxyImage, 'CData', galaxyColor(galaxyDensity));
      if isgraphics(galaxyInfo)
        set(galaxyInfo, 'String', sprintf(...
          '%d of %d stars binned from Parquet into a %dx%d density', ...
          galaxyProcessed, galaxyStarCount, galaxyBins, galaxyBins));
      end
      drawnow();
      if galaxyProcessed >= galaxyStarCount
        if ~galaxyAnimationContinuous
          break
        end
        galaxyDensity = zeros(galaxyBins, galaxyBins);
        galaxyProcessed = 0;
      end
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if galaxyFramePause > 0
      pause(galaxyFramePause);
    end
  end
end
%=============================================================================
function index = galaxyBinIndex(values, limit, bins)
  index = round((values + limit) / (2 * limit) * (bins - 1)) + 1;
  index = min(max(index, 1), bins);
end
%=============================================================================
function rgb = galaxyColor(density)
  % Map a log-scaled star count to a dark-purple to gold nebula gradient.
  scaled = log(1 + density);
  peak = max(scaled(:));
  if peak <= 0
    peak = 1;
  end
  normalized = scaled / peak;
  rows = size(density, 1);
  cols = size(density, 2);
  rgb = zeros(rows, cols, 3);
  rgb(:, :, 1) = min(1, 0.1 + 1.6 * normalized);
  rgb(:, :, 2) = min(1, 0.6 * normalized .^ 1.4);
  rgb(:, :, 3) = min(1, 0.25 + 1.1 * normalized .^ 0.7);
end
%=============================================================================
