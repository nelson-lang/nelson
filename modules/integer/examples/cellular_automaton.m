%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Stream a one-dimensional elementary cellular automaton stored in uint8 cells.
if ~exist('caFigureVisible', 'var')
  caFigureVisible = 'on';
end
if ~exist('caAnimationEnabled', 'var')
  caAnimationEnabled = true;
end
if ~exist('caAnimationFrames', 'var')
  caAnimationFrames = 700;
end
if ~exist('caFramePause', 'var')
  caFramePause = 0;
end
if ~exist('caAnimationContinuous', 'var')
  caAnimationContinuous = strcmp(caFigureVisible, 'on');
end
if ~exist('caWidth', 'var')
  caWidth = 601;
end
if ~exist('caHeight', 'var')
  caHeight = 320;
end
% Wolfram rule number as a uint8: 30 is chaotic, 90 draws Sierpinski, 110
% is computationally universal.
if ~exist('caRule', 'var')
  caRule = uint8(30);
end

caRule = uint8(caRule);
% Precompute the 8-entry transition table of the rule once, in uint8.
caNeighborhoods = uint8(0:7);
caRuleTable = uint8(mod(floor(double(caRule) ./ 2 .^ double(caNeighborhoods)), 2));
caRow = zeros(1, caWidth, 'uint8');
caRow(ceil(caWidth / 2)) = uint8(1);
caBuffer = zeros(caHeight, caWidth, 'uint8');
caBuffer(end, :) = caRow;

caAliveColor = [0.30 0.92 0.76];
caDeadColor = [0.05 0.06 0.10];

caFigure = figure('Name', 'Elementary cellular automaton', ...
  'NumberTitle', 'off', 'Color', [0.05 0.06 0.10], ...
  'Visible', caFigureVisible, 'Position', [90 90 940 620]);
caAxes = axes('Parent', caFigure, 'Position', [0.04 0.10 0.92 0.84], ...
  'Color', [0.05 0.06 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(caAxes);
caImage = image(caAxes, 'CData', caColorize(caBuffer, caAliveColor, ...
  caDeadColor));
axis(caAxes, 'off');
set(caAxes, 'YDir', 'reverse');
caTitle = title(caAxes, sprintf('Rule %d', caRule), ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
caInfoText = uicontrol('Parent', caFigure, 'Style', 'text', 'String', '', ...
  'Position', [16 8 820 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.10], 'ForegroundColor', [0.72 0.86 0.96], ...
  'FontSize', 10);

if caAnimationEnabled
  caFrame = 0;
  while isgraphics(caFigure)
    caFrame = caFrame + 1;
    if caFrame > caAnimationFrames && ~caAnimationContinuous
      break
    end
    if ~isgraphics(caFigure)
      break
    end
    try
      caClock = tic();
      caRow = caNextGeneration(caRow, caRuleTable);
      % Scroll the picture up by one line and draw the new generation at
      % the bottom, so the automaton streams downward forever.
      caBuffer(1:end - 1, :) = caBuffer(2:end, :);
      caBuffer(end, :) = caRow;
      set(caImage, 'CData', caColorize(caBuffer, caAliveColor, caDeadColor));
      caElapsed = toc(caClock);
      if isgraphics(caInfoText) && caElapsed > 0
        set(caInfoText, 'String', sprintf(...
          'Rule %d | %d cells/generation | generation %d | %.0f gen/s', ...
          caRule, caWidth, caFrame, 1 / caElapsed));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if caFramePause > 0
      pause(caFramePause);
    end
  end
end
%=============================================================================
function nextRow = caNextGeneration(row, ruleTable)
  % Wrap the edges so the lattice is a ring.
  leftNeighbor = [row(end) row(1:end - 1)];
  rightNeighbor = [row(2:end) row(1)];
  % Encode each 3-cell neighbourhood as an index 0..7 and look up the rule.
  index = 4 * leftNeighbor + 2 * row + rightNeighbor;
  nextRow = ruleTable(double(index) + 1);
end
%=============================================================================
function rgb = caColorize(buffer, aliveColor, deadColor)
  alive = double(buffer);
  dead = 1 - alive;
  rgb = zeros(size(buffer, 1), size(buffer, 2), 3);
  for channel = 1:3
    rgb(:, :, channel) = alive * aliveColor(channel) + ...
      dead * deadColor(channel);
  end
end
%=============================================================================
