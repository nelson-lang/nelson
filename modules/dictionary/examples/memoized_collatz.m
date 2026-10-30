%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Reveal the Collatz stopping times, memoized in a dictionary as they appear.
if ~exist('collatzFigureVisible', 'var')
  collatzFigureVisible = 'on';
end
if ~exist('collatzAnimationEnabled', 'var')
  collatzAnimationEnabled = true;
end
if ~exist('collatzFramePause', 'var')
  collatzFramePause = 0;
end
if ~exist('collatzAnimationContinuous', 'var')
  collatzAnimationContinuous = strcmp(collatzFigureVisible, 'on');
end
if ~exist('collatzMax', 'var')
  collatzMax = 12000;
end
if ~exist('collatzPerFrame', 'var')
  collatzPerFrame = 20;
end

% The dictionary caches the stopping time of every integer it ever visits,
% including the large intermediate values on each Collatz orbit.
collatzCache = configureDictionary('double', 'double');
collatzCache(1) = 0;
collatzNext = 2;
collatzPointsX = nan(1, collatzMax);
collatzPointsY = nan(1, collatzMax);

collatzFigure = figure('Name', 'Memoized Collatz', 'NumberTitle', 'off', ...
  'Color', [0.05 0.06 0.10], 'Visible', collatzFigureVisible, ...
  'Position', [100 110 1060 560]);
collatzAxes = axes('Parent', collatzFigure, 'Position', ...
  [0.07 0.13 0.88 0.78], 'Color', [0.05 0.06 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(collatzAxes);
collatzScatter = plot(collatzAxes, nan, nan, '.', 'Color', ...
  [0.35 0.85 1.00], 'MarkerSize', 4, 'LineStyle', 'none');
set(collatzAxes, 'XColor', [0.6 0.66 0.78], 'YColor', [0.6 0.66 0.78]);
set(collatzAxes, 'XLim', [0 collatzMax], 'YLim', [0 320]);
grid(collatzAxes, 'on');
xlabel(collatzAxes, 'starting number n');
ylabel(collatzAxes, 'steps to reach 1');
title(collatzAxes, 'Collatz stopping times, memoized in a dictionary', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
collatzInfo = uicontrol('Parent', collatzFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 940 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.10], 'ForegroundColor', [0.76 0.84 0.96], ...
  'FontSize', 10);

if collatzAnimationEnabled
  while isgraphics(collatzFigure)
    if ~isgraphics(collatzFigure)
      break
    end
    try
      % Compute and memoize a batch of stopping times this frame.
      for repeat = 1:collatzPerFrame
        if collatzNext > collatzMax
          break
        end
        [collatzCache, stopTime] = collatzStoppingTime(collatzCache, ...
          collatzNext);
        collatzPointsX(collatzNext) = collatzNext;
        collatzPointsY(collatzNext) = stopTime;
        collatzNext = collatzNext + 1;
      end
      set(collatzScatter, 'XData', collatzPointsX, 'YData', collatzPointsY);
      if isgraphics(collatzInfo)
        set(collatzInfo, 'String', sprintf(...
          'resolved %d / %d numbers | %d entries cached in the dictionary', ...
          min(collatzNext - 1, collatzMax), collatzMax, ...
          numEntries(collatzCache)));
      end
      drawnow();
      if collatzNext > collatzMax && ~collatzAnimationContinuous
        break
      end
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if collatzFramePause > 0
      pause(collatzFramePause);
    end
  end
end
%=============================================================================
function [cache, stopTime] = collatzStoppingTime(cache, number)
  % Walk the orbit until a cached value, then backfill every visited number.
  path = [];
  value = number;
  while ~isKey(cache, value)
    path(end + 1) = value;
    if mod(value, 2) == 0
      value = value / 2;
    else
      value = 3 * value + 1;
    end
  end
  steps = cache(value);
  for index = numel(path):-1:1
    steps = steps + 1;
    cache(path(index)) = steps;
  end
  stopTime = cache(number);
end
%=============================================================================
