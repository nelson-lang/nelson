%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Profile a mixed workload and reveal where the time went as growing bars.
if ~exist('profFigureVisible', 'var')
  profFigureVisible = 'on';
end
if ~exist('profAnimationEnabled', 'var')
  profAnimationEnabled = true;
end
if ~exist('profAnimationFrames', 'var')
  profAnimationFrames = 90;
end
if ~exist('profFramePause', 'var')
  profFramePause = 0.02;
end
if ~exist('profAnimationContinuous', 'var')
  profAnimationContinuous = false;
end

% Run a mixed workload under the profiler and collect the function table.
profile('on');
profileRunWorkload();
profile('off');
profInfo = profile('info');
profTimes = [profInfo.TotalTime];
profNames = {profInfo.FunctionName};
[profSortedTimes, profOrder] = sort(profTimes, 'descend');
profTopCount = min(9, numel(profOrder));
profTopTimes = profSortedTimes(1:profTopCount);
profTopNames = profNames(profOrder(1:profTopCount));
profMaxTime = max(profTopTimes);
if profMaxTime <= 0
  profMaxTime = 1;
end

profFigure = figure('Name', 'Profiler hotspots', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.10], 'Visible', profFigureVisible, ...
  'Position', [120 110 1000 560]);
profAxes = axes('Parent', profFigure, 'Position', [0.28 0.10 0.68 0.82], ...
  'Color', [0.06 0.07 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(profAxes);
hold(profAxes, 'on');
profBars = gobjects(1, profTopCount);
profValueLabels = gobjects(1, profTopCount);
profPalette = profColors(profTopCount);
for barIndex = 1:profTopCount
  rowY = profTopCount - barIndex + 1;
  profBars(barIndex) = patch('Parent', profAxes, 'XData', ...
    [0 0 0 0], 'YData', [rowY - 0.4 rowY - 0.4 rowY + 0.4 rowY + 0.4], ...
    'FaceColor', profPalette(barIndex, :), 'EdgeColor', 'none');
  text(profAxes, -0.02 * profMaxTime, rowY, profTopNames{barIndex}, ...
    'HorizontalAlignment', 'right', 'Color', [0.85 0.90 0.98], ...
    'FontSize', 10, 'Interpreter', 'none');
  profValueLabels(barIndex) = text(profAxes, 0, rowY, '', ...
    'HorizontalAlignment', 'left', 'Color', [0.95 0.96 1.00], ...
    'FontSize', 10);
end
hold(profAxes, 'off');
set(profAxes, 'YLim', [0.3 profTopCount + 0.7], 'XLim', ...
  [0 profMaxTime * 1.15], 'YTick', [], 'XColor', [0.6 0.66 0.78], ...
  'YColor', [0.06 0.07 0.10]);
xlabel(profAxes, 'total time (s)');
title(profAxes, 'Where the profiled workload spent its time', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);

if profAnimationEnabled
  profFrame = 0;
  while isgraphics(profFigure)
    profFrame = profFrame + 1;
    if profFrame > profAnimationFrames && ~profAnimationContinuous
      break
    end
    if ~isgraphics(profFigure)
      break
    end
    try
      % Ease every bar from zero toward its measured time.
      progress = min(1, profFrame / profAnimationFrames);
      eased = 1 - (1 - progress) ^ 3;
      for barIndex = 1:profTopCount
        rowY = profTopCount - barIndex + 1;
        width = eased * profTopTimes(barIndex);
        set(profBars(barIndex), 'XData', [0 width width 0]);
        if isgraphics(profValueLabels(barIndex))
          set(profValueLabels(barIndex), 'Position', ...
            [width + 0.01 * profMaxTime rowY 0], 'String', ...
            sprintf('%.1f ms', 1000 * width));
        end
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if profFramePause > 0
      pause(profFramePause);
    end
  end
end
%=============================================================================
function profileRunWorkload()
  for round = 1:3
    profileSortWork();
    profileMatrixWork();
    profileTrigWork();
    profileLoopWork();
  end
end
%=============================================================================
function profileSortWork()
  data = rand(1, 120000);
  sorted = sort(data);
  sorted = sort(sorted, 'descend');
end
%=============================================================================
function profileMatrixWork()
  a = rand(200, 200);
  b = a * a.';
  c = b + a;
end
%=============================================================================
function profileTrigWork()
  x = linspace(0, 100, 60000);
  y = sin(x) + cos(2 * x);
end
%=============================================================================
function profileLoopWork()
  total = 0;
  for index = 1:14000
    total = total + sqrt(index);
  end
end
%=============================================================================
function colors = profColors(count)
  colors = zeros(count, 3);
  for index = 1:count
    shade = 0.45 + 0.5 * (1 - index / count);
    colors(index, :) = [1.00 * shade, 0.55 * shade, 0.30 * shade];
  end
end
%=============================================================================
