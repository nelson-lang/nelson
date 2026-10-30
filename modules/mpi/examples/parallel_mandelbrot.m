%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compute a Mandelbrot set across MPI ranks and reveal every band at once.
if ~exist('pmFigureVisible', 'var')
  pmFigureVisible = 'on';
end
if ~exist('pmAnimationEnabled', 'var')
  pmAnimationEnabled = true;
end
if ~exist('pmAnimationFrames', 'var')
  pmAnimationFrames = 90;
end
if ~exist('pmFramePause', 'var')
  pmFramePause = 0.02;
end
if ~exist('pmAnimationContinuous', 'var')
  pmAnimationContinuous = strcmp(pmFigureVisible, 'on');
end
if ~exist('pmProcessCount', 'var')
  pmProcessCount = 4;
end

% Launch the MPI workers; each rank fills a band and rank 0 saves the image.
pmWorker = [modulepath('mpi', 'root'), '/examples/parallel_mandelbrot_worker.m'];
pmClock = tic();
[pmStatus, pmOutput] = mpiexec(pmWorker, pmProcessCount);
pmElapsed = toc(pmClock);
% MPI workers must launch here; stop clearly when the launcher is missing.
if pmStatus ~= 0
  error('Nelson:parallel_mandelbrot:mpiLaunchFailed', ...
    'This demo needs a working MPI launcher (see doc mpiexec).');
end
pmResultFile = [tempdir(), 'nelson_mpi_mandelbrot.mat'];
pmLoaded = loadmat(pmResultFile);
pmEscape = pmLoaded.escapeImage;
pmRankOwner = pmLoaded.rankOwner;
pmRanks = pmLoaded.mpiResultRanks;
pmMaxIterations = pmLoaded.mpiResultMaxIterations;
[pmHeight, pmWidth] = size(pmEscape);
pmColormap = pmPalette(pmMaxIterations + 1);
pmTrueColor = pmApplyColormap(pmEscape, pmColormap, pmMaxIterations);
pmRankColors = pmRankPalette(pmRanks);

pmFigure = figure('Name', 'MPI Mandelbrot', 'NumberTitle', 'off', ...
  'Color', [0.02 0.02 0.05], 'Visible', pmFigureVisible, ...
  'Position', [100 70 820 820]);
pmAxes = axes('Parent', pmFigure, 'Position', [0.04 0.08 0.92 0.86]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(pmAxes);
pmImage = image(pmAxes, 'CData', zeros(pmHeight, pmWidth, 3));
axis(pmAxes, 'image');
axis(pmAxes, 'off');
set(pmAxes, 'YDir', 'reverse');
title(pmAxes, sprintf('Mandelbrot computed across %d MPI ranks', pmRanks), ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
pmInfo = uicontrol('Parent', pmFigure, 'Style', 'text', 'String', ...
  sprintf('%d ranks via mpiexec in %.2f s | each colour strip is one rank', ...
  pmRanks, pmElapsed), 'Position', [16 8 760 20], 'HorizontalAlignment', ...
  'left', 'BackgroundColor', [0.02 0.02 0.05], 'ForegroundColor', ...
  [0.80 0.86 0.96], 'FontSize', 10);

pmRowsPerRank = ceil(pmHeight / pmRanks);
if pmAnimationEnabled
  pmFrame = 0;
  while isgraphics(pmFigure)
    pmFrame = pmFrame + 1;
    if pmFrame > pmAnimationFrames && ~pmAnimationContinuous
      break
    end
    if ~isgraphics(pmFigure)
      break
    end
    try
      % Every band fills from its own top at the same rate: all ranks at once.
      progress = min(1, pmFrame / (pmAnimationFrames * 0.7));
      display = zeros(pmHeight, pmWidth, 3);
      for rankIndex = 0:pmRanks - 1
        bandFirst = rankIndex * pmRowsPerRank + 1;
        bandLast = min((rankIndex + 1) * pmRowsPerRank, pmHeight);
        if bandFirst > pmHeight
          continue
        end
        revealed = bandFirst + round(progress * (bandLast - bandFirst));
        rows = bandFirst:revealed;
        display(rows, :, :) = pmTrueColor(rows, :, :);
        % A thin colour strip on the left marks which rank owns the band.
        display(bandFirst:bandLast, 1:8, 1) = pmRankColors(rankIndex + 1, 1);
        display(bandFirst:bandLast, 1:8, 2) = pmRankColors(rankIndex + 1, 2);
        display(bandFirst:bandLast, 1:8, 3) = pmRankColors(rankIndex + 1, 3);
      end
      set(pmImage, 'CData', display);
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if pmFramePause > 0
      pause(pmFramePause);
    end
  end
end
%=============================================================================
function colormap = pmPalette(levels)
  t = linspace(0, 1, levels).';
  colormap = [0.5 + 0.5 * sin(6.2 * t + 0.4), ...
    0.5 + 0.5 * sin(6.2 * t + 2.3), 0.5 + 0.5 * sin(6.2 * t + 4.1)];
end
%=============================================================================
function colors = pmRankPalette(count)
  colors = zeros(count, 3);
  for index = 1:count
    hue = (index - 1) / count;
    colors(index, :) = [0.5 + 0.5 * sin(hue * 6.28), ...
      0.5 + 0.5 * sin(hue * 6.28 + 2.1), 0.5 + 0.5 * sin(hue * 6.28 + 4.2)];
  end
end
%=============================================================================
function imageData = pmApplyColormap(grid, colormap, maxIter)
  levels = size(colormap, 1);
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
  inSet = grid >= maxIter - 0.5;
  for channel = 1:3
    plane = imageData(:, :, channel);
    plane(inSet) = 0;
    imageData(:, :, channel) = plane;
  end
end
%=============================================================================
