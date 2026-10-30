%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Wind the integers along a square spiral and light up the primes.
if ~exist('ulamFigureVisible', 'var')
  ulamFigureVisible = 'on';
end
if ~exist('ulamAnimationEnabled', 'var')
  ulamAnimationEnabled = true;
end
if ~exist('ulamAnimationFrames', 'var')
  ulamAnimationFrames = 240;
end
if ~exist('ulamFramePause', 'var')
  ulamFramePause = 0;
end
if ~exist('ulamAnimationContinuous', 'var')
  ulamAnimationContinuous = strcmp(ulamFigureVisible, 'on');
end
if ~exist('ulamCount', 'var')
  ulamCount = 40401;
end

[ulamX, ulamY] = ulamSpiralCoords(ulamCount);
% A sieve from primes() marks which spiral cells carry a prime.
ulamIsPrime = false(1, ulamCount);
ulamIsPrime(primes(ulamCount)) = true;
ulamPrimeIndices = find(ulamIsPrime);
ulamFrameCount = max(2, ulamAnimationFrames);
ulamSpan = max(abs([ulamX ulamY])) + 1;

ulamFigure = figure('Name', 'Ulam spiral of primes', 'NumberTitle', 'off', ...
  'Color', [0.03 0.04 0.07], 'Visible', ulamFigureVisible, ...
  'Position', [110 70 840 840]);
ulamAxes = axes('Parent', ulamFigure, 'Position', [0.04 0.08 0.92 0.86], ...
  'Color', [0.03 0.04 0.07]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(ulamAxes);
hold(ulamAxes, 'on');
ulamScatter = plot(ulamAxes, nan, nan, '.', 'Color', [0.45 0.90 1.00], ...
  'MarkerSize', 6, 'LineStyle', 'none');
hold(ulamAxes, 'off');
axis(ulamAxes, 'equal');
axis(ulamAxes, 'off');
set(ulamAxes, 'XLim', [-ulamSpan ulamSpan], 'YLim', [-ulamSpan ulamSpan]);
ulamTitle = title(ulamAxes, 'Ulam spiral: primes fall on diagonal streaks', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
ulamInfoText = uicontrol('Parent', ulamFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 800 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.03 0.04 0.07], 'ForegroundColor', [0.70 0.86 0.98], ...
  'FontSize', 10);

if ulamAnimationEnabled
  while isgraphics(ulamFigure)
    for ulamFrameIndex = 1:ulamFrameCount
      if ~isgraphics(ulamFigure)
        break
      end
      try
        % Reveal the spiral outward; show every prime drawn so far.
        visibleCount = round(ulamCount * ulamFrameIndex / ulamFrameCount);
        shown = ulamPrimeIndices(ulamPrimeIndices <= visibleCount);
        set(ulamScatter, 'XData', ulamX(shown), 'YData', ulamY(shown));
        if isgraphics(ulamInfoText)
          set(ulamInfoText, 'String', sprintf(...
            'integers 1..%d | %d primes placed', visibleCount, numel(shown)));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if ulamFramePause > 0
        pause(ulamFramePause);
      end
    end
    if ~ulamAnimationContinuous || ~isgraphics(ulamFigure)
      break
    end
  end
end
%=============================================================================
function [spiralX, spiralY] = ulamSpiralCoords(count)
  spiralX = zeros(1, count);
  spiralY = zeros(1, count);
  directions = [1 0; 0 1; -1 0; 0 -1];
  x = 0;
  y = 0;
  directionIndex = 1;
  stepLength = 1;
  legsAtLength = 0;
  position = 2;
  while position <= count
    for step = 1:stepLength
      x = x + directions(directionIndex, 1);
      y = y + directions(directionIndex, 2);
      spiralX(position) = x;
      spiralY(position) = y;
      position = position + 1;
      if position > count
        break
      end
    end
    directionIndex = mod(directionIndex, 4) + 1;
    legsAtLength = legsAtLength + 1;
    if legsAtLength == 2
      legsAtLength = 0;
      stepLength = stepLength + 1;
    end
  end
end
%=============================================================================
