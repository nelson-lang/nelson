%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sample a shifting weather mix into a categorical array and race the counts.
if ~exist('catDistFigureVisible', 'var')
  catDistFigureVisible = 'on';
end
if ~exist('catDistAnimationEnabled', 'var')
  catDistAnimationEnabled = true;
end
if ~exist('catDistAnimationFrames', 'var')
  catDistAnimationFrames = 700;
end
if ~exist('catDistFramePause', 'var')
  catDistFramePause = 0.02;
end
if ~exist('catDistAnimationContinuous', 'var')
  catDistAnimationContinuous = strcmp(catDistFigureVisible, 'on');
end
if ~exist('catDistSampleSize', 'var')
  catDistSampleSize = 400;
end
rng(7);

catDistNames = {'Sunny', 'Cloudy', 'Rainy', 'Snowy', 'Foggy'};
catDistCount = numel(catDistNames);
catDistColors = [0.98 0.80 0.25; 0.60 0.66 0.74; 0.30 0.55 0.95; ...
  0.85 0.92 0.98; 0.70 0.74 0.62];

catDistFigure = figure('Name', 'Weather categories', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.10], 'Visible', catDistFigureVisible, ...
  'Position', [120 110 900 560]);
catDistAxes = axes('Parent', catDistFigure, 'Position', ...
  [0.08 0.14 0.88 0.76], 'Color', [0.06 0.07 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(catDistAxes);
hold(catDistAxes, 'on');
catDistBars = gobjects(1, catDistCount);
catDistValueLabels = gobjects(1, catDistCount);
for barIndex = 1:catDistCount
  catDistBars(barIndex) = patch('Parent', catDistAxes, 'XData', ...
    [barIndex - 0.4 barIndex + 0.4 barIndex + 0.4 barIndex - 0.4], ...
    'YData', [0 0 0 0], 'FaceColor', catDistColors(barIndex, :), ...
    'EdgeColor', 'none');
  catDistValueLabels(barIndex) = text(catDistAxes, barIndex, 0, '', ...
    'HorizontalAlignment', 'center', 'Color', [0.92 0.94 0.98], ...
    'FontSize', 10);
end
hold(catDistAxes, 'off');
set(catDistAxes, 'XLim', [0.4 catDistCount + 0.6], 'YLim', ...
  [0 catDistSampleSize], 'XTick', 1:catDistCount, 'XTickLabel', ...
  catDistNames, 'XColor', [0.7 0.76 0.86], 'YColor', [0.7 0.76 0.86]);
ylabel(catDistAxes, 'count in sample');
title(catDistAxes, 'Weather sample classified as a categorical array', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
catDistInfo = uicontrol('Parent', catDistFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 820 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.06 0.07 0.10], 'ForegroundColor', [0.76 0.84 0.96], ...
  'FontSize', 10);

if catDistAnimationEnabled
  catDistFrame = 0;
  while isgraphics(catDistFigure)
    catDistFrame = catDistFrame + 1;
    if catDistFrame > catDistAnimationFrames && ~catDistAnimationContinuous
      break
    end
    if ~isgraphics(catDistFigure)
      break
    end
    try
      % The category weights drift smoothly, so the mix keeps shifting.
      phase = catDistFrame * 0.03;
      weights = 1 + 0.9 * sin(phase + (1:catDistCount) * 1.2);
      probabilities = weights / sum(weights);
      edges = [0 cumsum(probabilities)];
      draws = rand(1, catDistSampleSize);
      sampleIndices = zeros(1, catDistSampleSize);
      for category = 1:catDistCount
        inBin = draws >= edges(category) & draws < edges(category + 1);
        sampleIndices(inBin) = category;
      end
      sample = categorical(catDistNames(sampleIndices), catDistNames);
      topCount = 0;
      for barIndex = 1:catDistCount
        % Count members of each category with the categorical == operator.
        count = sum(sample == catDistNames{barIndex});
        set(catDistBars(barIndex), 'YData', [0 0 count count]);
        set(catDistValueLabels(barIndex), 'Position', ...
          [barIndex count + catDistSampleSize * 0.02 0], 'String', ...
          sprintf('%d', count));
        topCount = max(topCount, count);
      end
      if isgraphics(catDistInfo)
        set(catDistInfo, 'String', sprintf(...
          '%d samples | %d categories | most common this frame has %d', ...
          catDistSampleSize, catDistCount, topCount));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if catDistFramePause > 0
      pause(catDistFramePause);
    end
  end
end
%=============================================================================
