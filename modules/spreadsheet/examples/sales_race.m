%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Write monthly sales to a CSV, read them back and race the product totals.
if ~exist('salesFigureVisible', 'var')
  salesFigureVisible = 'on';
end
if ~exist('salesAnimationEnabled', 'var')
  salesAnimationEnabled = true;
end
if ~exist('salesFramePause', 'var')
  salesFramePause = 0.08;
end
if ~exist('salesAnimationContinuous', 'var')
  salesAnimationContinuous = strcmp(salesFigureVisible, 'on');
end
if ~exist('salesMonths', 'var')
  salesMonths = 48;
end
rng(11);

salesProducts = {'Alpha', 'Bravo', 'Carbon', 'Delta', 'Echo', 'Falcon'};
salesProductCount = numel(salesProducts);
% Build cumulative monthly sales, one column per product, and write a CSV.
salesMonthly = rand(salesMonths, salesProductCount) .* ...
  (0.5 + (1:salesProductCount) / salesProductCount);
salesCumulative = cumsum(salesMonthly);
salesTable = array2table([(1:salesMonths).' salesCumulative], ...
  'VariableNames', [{'Month'} salesProducts]);
salesFile = [tempname(), '.csv'];
writetable(salesTable, salesFile);
% Read the sales history straight back from the CSV file.
salesLoaded = readtable(salesFile);
salesData = table2array(salesLoaded(:, 2:end));
salesColors = salesPalette(salesProductCount);

salesFigure = figure('Name', 'Sales race', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.10], 'Visible', salesFigureVisible, ...
  'Position', [120 100 940 560]);
salesAxes = axes('Parent', salesFigure, 'Position', [0.16 0.10 0.80 0.80], ...
  'Color', [0.06 0.07 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(salesAxes);
hold(salesAxes, 'on');
salesBars = gobjects(1, salesProductCount);
salesNameLabels = gobjects(1, salesProductCount);
salesValueLabels = gobjects(1, salesProductCount);
for barIndex = 1:salesProductCount
  salesBars(barIndex) = patch('Parent', salesAxes, 'XData', [0 0 0 0], ...
    'YData', [0 0 0 0], 'FaceColor', salesColors(barIndex, :), ...
    'EdgeColor', 'none');
  salesNameLabels(barIndex) = text(salesAxes, 0, 0, '', ...
    'HorizontalAlignment', 'right', 'Color', [0.85 0.90 0.98], ...
    'FontSize', 11);
  salesValueLabels(barIndex) = text(salesAxes, 0, 0, '', ...
    'HorizontalAlignment', 'left', 'Color', [0.95 0.96 1.00], ...
    'FontSize', 10);
end
hold(salesAxes, 'off');
set(salesAxes, 'YLim', [0.3 salesProductCount + 0.7], 'YTick', [], ...
  'XColor', [0.6 0.66 0.78], 'YColor', [0.06 0.07 0.10]);
xlabel(salesAxes, 'cumulative sales');
salesTitle = title(salesAxes, '', 'Color', [0.92 0.94 0.98], 'FontSize', 13);
salesInfo = uicontrol('Parent', salesFigure, 'Style', 'text', 'String', ...
  '', 'Position', [16 8 820 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.06 0.07 0.10], 'ForegroundColor', [0.76 0.84 0.96], ...
  'FontSize', 10);

if salesAnimationEnabled
  salesFrame = 0;
  while isgraphics(salesFigure)
    salesFrame = salesFrame + 1;
    month = mod(salesFrame - 1, salesMonths) + 1;
    if ~salesAnimationContinuous && salesFrame > salesMonths
      break
    end
    if ~isgraphics(salesFigure)
      break
    end
    try
      values = salesData(month, :);
      [sortedValues, order] = sort(values, 'descend');
      maxValue = max([sortedValues 1]);
      set(salesAxes, 'XLim', [0 maxValue * 1.2]);
      for rank = 1:salesProductCount
        rowY = salesProductCount - rank + 1;
        width = sortedValues(rank);
        set(salesBars(rank), 'XData', [0 width width 0], 'YData', ...
          [rowY - 0.4 rowY - 0.4 rowY + 0.4 rowY + 0.4], 'FaceColor', ...
          salesColors(order(rank), :));
        set(salesNameLabels(rank), 'Position', [-0.01 * maxValue rowY 0], ...
          'String', salesProducts{order(rank)});
        set(salesValueLabels(rank), 'Position', ...
          [width + 0.01 * maxValue rowY 0], 'String', ...
          sprintf('%.1f', width));
      end
      set(salesTitle, 'String', sprintf('Cumulative sales after month %d', ...
        month));
      if isgraphics(salesInfo)
        set(salesInfo, 'String', sprintf(...
          '%d months x %d products read from a CSV with readtable', ...
          salesMonths, salesProductCount));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if salesFramePause > 0
      pause(salesFramePause);
    end
  end
end
%=============================================================================
function colors = salesPalette(count)
  colors = zeros(count, 3);
  for index = 1:count
    hue = (index - 1) / count;
    colors(index, :) = [0.4 + 0.5 * sin(hue * 6.28), ...
      0.5 + 0.4 * sin(hue * 6.28 + 2), 0.6 + 0.4 * sin(hue * 6.28 + 4)];
  end
  colors = min(max(colors, 0.15), 1);
end
%=============================================================================
