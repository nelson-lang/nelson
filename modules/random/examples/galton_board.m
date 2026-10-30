%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Drop balls through a peg board and watch the bins fill into a bell curve.
if ~exist('galtonFigureVisible', 'var')
  galtonFigureVisible = 'on';
end
if ~exist('galtonAnimationEnabled', 'var')
  galtonAnimationEnabled = true;
end
if ~exist('galtonAnimationFrames', 'var')
  galtonAnimationFrames = 900;
end
if ~exist('galtonFramePause', 'var')
  galtonFramePause = 0;
end
if ~exist('galtonAnimationContinuous', 'var')
  galtonAnimationContinuous = strcmp(galtonFigureVisible, 'on');
end
if ~exist('galtonRows', 'var')
  galtonRows = 14;
end
if ~exist('galtonSoundEnabled', 'var')
  galtonSoundEnabled = strcmp(galtonFigureVisible, 'on');
end
if ~exist('galtonSeed', 'var')
  galtonSeed = 20261030;
end
rng(galtonSeed);

galtonBinCount = galtonRows + 1;
galtonBinCounts = zeros(1, galtonBinCount);
galtonSpacing = 1.0;
galtonRowHeight = 1.0;
galtonTopY = galtonRows * galtonRowHeight + 2;
% Peg centres form the classic triangular lattice.
[galtonPegX, galtonPegY] = galtonPegLayout(galtonRows, galtonSpacing, ...
  galtonRowHeight);
galtonBinCenters = ((0:galtonRows) - galtonRows / 2) * galtonSpacing;

galtonFigure = figure('Name', 'Galton board', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.10], 'Visible', galtonFigureVisible, ...
  'Position', [100 70 760 880]);
galtonAxes = axes('Parent', galtonFigure, 'Position', [0.07 0.06 0.9 0.9], ...
  'Color', [0.06 0.07 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(galtonAxes);
hold(galtonAxes, 'on');
plot(galtonAxes, galtonPegX, galtonPegY, 'o', 'MarkerSize', 4, ...
  'MarkerFaceColor', [0.40 0.45 0.55], 'MarkerEdgeColor', 'none', ...
  'LineStyle', 'none');
galtonBinWidth = galtonSpacing * 0.82;
galtonBars = bar(galtonAxes, galtonBinCenters, galtonBinCounts, 0.82, ...
  'FaceColor', [0.26 0.62 0.95], 'EdgeColor', 'none');
galtonBallScatter = plot(galtonAxes, nan, nan, 'o', 'MarkerSize', 7, ...
  'MarkerFaceColor', [1.00 0.78 0.22], 'MarkerEdgeColor', [0.2 0.15 0.0], ...
  'LineStyle', 'none');
galtonNormalLine = plot(galtonAxes, nan, nan, 'Color', [0.95 0.45 0.35], ...
  'LineWidth', 2.0);
hold(galtonAxes, 'off');
set(galtonAxes, 'XColor', [0.7 0.74 0.82], 'YColor', [0.7 0.74 0.82]);
galtonHalfSpan = (galtonRows / 2 + 1.5) * galtonSpacing;
set(galtonAxes, 'XLim', [-galtonHalfSpan galtonHalfSpan], ...
  'YLim', [-0.5 galtonTopY + 1]);
title(galtonAxes, 'Galton board: randomness builds a bell curve', ...
  'Color', [0.92 0.94 0.98]);
galtonInfoText = uicontrol('Parent', galtonFigure, 'Style', 'text', ...
  'String', 'dropping...', 'Position', [16 8 700 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.06 0.07 0.10], ...
  'ForegroundColor', [0.75 0.82 0.95], 'FontSize', 10);

% Prepare a soft click played whenever a ball settles into a bin.
[galtonClick, galtonClickRate] = galtonClickSample();
galtonPlayer = [];

% Balls currently falling, tracked by their peg row and running right-count.
galtonLiveRow = [];
galtonLiveRights = [];
galtonLiveX = [];
galtonDropped = 0;
galtonLanded = 0;

if galtonAnimationEnabled
  galtonFrame = 0;
  while isgraphics(galtonFigure)
    galtonFrame = galtonFrame + 1;
    if galtonFrame > galtonAnimationFrames && ~galtonAnimationContinuous
      break
    end
    if ~isgraphics(galtonFigure)
      break
    end
    try
      % Launch a fresh ball from the top on most frames.
      if mod(galtonFrame, 2) == 1
        galtonLiveRow(end + 1) = 0;
        galtonLiveRights(end + 1) = 0;
        galtonLiveX(end + 1) = 0;
        galtonDropped = galtonDropped + 1;
      end
      % Advance every live ball one row; a fair coin sends it left or right.
      steps = rand(1, numel(galtonLiveRow)) < 0.5;
      galtonLiveRights = galtonLiveRights + steps;
      galtonLiveRow = galtonLiveRow + 1;
      galtonLiveX = (galtonLiveRights - galtonLiveRow / 2) * galtonSpacing;
      galtonBallY = galtonTopY - galtonLiveRow * galtonRowHeight;
      % Balls past the last row settle into their bin.
      settled = galtonLiveRow > galtonRows;
      if any(settled)
        landedBins = galtonLiveRights(settled) + 1;
        for binIndex = landedBins
          galtonBinCounts(binIndex) = galtonBinCounts(binIndex) + 1;
        end
        galtonLanded = galtonLanded + numel(landedBins);
        galtonPlayer = galtonPlayClick(galtonSoundEnabled, galtonPlayer, ...
          galtonClick, galtonClickRate);
      end
      galtonLiveRow = galtonLiveRow(~settled);
      galtonLiveRights = galtonLiveRights(~settled);
      galtonLiveX = galtonLiveX(~settled);
      galtonBallY = galtonBallY(~settled);
      set(galtonBallScatter, 'XData', galtonLiveX, 'YData', galtonBallY);
      set(galtonBars, 'YData', galtonBinCounts);
      galtonTopCount = max(galtonBinCounts);
      if galtonTopCount > 0
        set(galtonAxes, 'YLim', [-0.5 max(galtonTopY + 1, ...
          galtonTopCount * 1.15)]);
        galtonNormalX = linspace(galtonBinCenters(1), ...
          galtonBinCenters(end), 160);
        galtonNormalY = galtonNormalCurve(galtonNormalX, galtonRows, ...
          galtonLanded);
        set(galtonNormalLine, 'XData', galtonNormalX, ...
          'YData', galtonNormalY);
      end
      if isgraphics(galtonInfoText)
        set(galtonInfoText, 'String', sprintf(...
          'dropped %d | settled %d | live %d', galtonDropped, ...
          galtonLanded, numel(galtonLiveRow)));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if galtonFramePause > 0
      pause(galtonFramePause);
    end
    if galtonFrame > galtonAnimationFrames && numel(galtonLiveRow) == 0
      if ~galtonAnimationContinuous
        break
      end
    end
  end
end
galtonStopPlayer(galtonPlayer);
%=============================================================================
function [pegX, pegY] = galtonPegLayout(rows, spacing, rowHeight)
  pegX = [];
  pegY = [];
  for rowIndex = 1:rows
    centers = ((0:rowIndex - 1) - (rowIndex - 1) / 2) * spacing;
    pegX = [pegX centers];
    pegY = [pegY repmat(rows * rowHeight + 2 - rowIndex * rowHeight, ...
      1, rowIndex)];
  end
end
%=============================================================================
function y = galtonNormalCurve(x, rows, total)
  % Normal approximation of the binomial peg counts, scaled to the samples.
  variance = rows / 4;
  if variance <= 0
    y = zeros(size(x));
    return
  end
  y = total / sqrt(2 * pi * variance) * exp(-(x .^ 2) / (2 * variance));
end
%=============================================================================
function [sample, rate] = galtonClickSample()
  rate = 22050;
  t = (0:round(0.03 * rate) - 1) / rate;
  sample = sin(2 * pi * 900 * t) .* exp(-t * 90);
  sample = 0.25 * sample(:);
end
%=============================================================================
function player = galtonPlayClick(enabled, player, sample, rate)
  if ~enabled || exist('audioplayer', 'builtin') == 0
    return
  end
  try
    player = audioplayer(sample, rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function galtonStopPlayer(player)
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
