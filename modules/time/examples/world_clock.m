%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Tick several analog clocks at once from the live clock and hour offsets.
if ~exist('worldClockFigureVisible', 'var')
  worldClockFigureVisible = 'on';
end
if ~exist('worldClockAnimationEnabled', 'var')
  worldClockAnimationEnabled = true;
end
if ~exist('worldClockAnimationFrames', 'var')
  worldClockAnimationFrames = 2000;
end
if ~exist('worldClockFramePause', 'var')
  worldClockFramePause = 0.05;
end
if ~exist('worldClockAnimationContinuous', 'var')
  worldClockAnimationContinuous = strcmp(worldClockFigureVisible, 'on');
end

% Each clock is a label and an offset in hours from the local machine time.
worldClockNames = {'Local', 'London +0', 'Tokyo +9', 'New York -5'};
worldClockOffsets = [0 0 9 -5];
worldClockCount = numel(worldClockNames);

worldClockFigure = figure('Name', 'World clock', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.11], 'Visible', worldClockFigureVisible, ...
  'Position', [100 120 1120 340]);
worldClockHands = cell(1, worldClockCount);
worldClockTitles = gobjects(1, worldClockCount);
for clockIndex = 1:worldClockCount
  axesHandle = axes('Parent', worldClockFigure, 'Position', ...
    [(clockIndex - 1) / worldClockCount + 0.01 0.06 ...
    1 / worldClockCount - 0.02 0.80]);
  % Prevent accidental pan or zoom from disturbing the live animation.
  disableDefaultInteractivity(axesHandle);
  worldClockDrawFace(axesHandle);
  worldClockHands{clockIndex} = worldClockCreateHands(axesHandle);
  worldClockTitles(clockIndex) = title(axesHandle, worldClockNames{clockIndex}, ...
    'Color', [0.90 0.93 0.98], 'FontSize', 12);
end

if worldClockAnimationEnabled
  worldClockFrame = 0;
  while isgraphics(worldClockFigure)
    worldClockFrame = worldClockFrame + 1;
    if worldClockFrame > worldClockAnimationFrames && ...
        ~worldClockAnimationContinuous
      break
    end
    if ~isgraphics(worldClockFigure)
      break
    end
    try
      localNow = clock();
      for clockIndex = 1:worldClockCount
        % Shift the live time by the city offset through serial-date math.
        shifted = datevec(datenum(localNow) + ...
          worldClockOffsets(clockIndex) / 24);
        worldClockUpdateHands(worldClockHands{clockIndex}, shifted);
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if worldClockFramePause > 0
      pause(worldClockFramePause);
    end
  end
end
%=============================================================================
function worldClockDrawFace(axesHandle)
  hold(axesHandle, 'on');
  theta = linspace(0, 2 * pi, 200);
  plot(axesHandle, cos(theta), sin(theta), 'Color', [0.55 0.62 0.75], ...
    'LineWidth', 2.0);
  % Twelve hour ticks around the rim.
  inner = 0.86;
  for tick = 0:11
    tickAngle = pi / 2 - tick * pi / 6;
    plot(axesHandle, [inner * cos(tickAngle) cos(tickAngle)], ...
      [inner * sin(tickAngle) sin(tickAngle)], 'Color', [0.6 0.67 0.8], ...
      'LineWidth', 1.6);
  end
  hold(axesHandle, 'off');
  axis(axesHandle, 'equal');
  axis(axesHandle, 'off');
  set(axesHandle, 'XLim', [-1.15 1.15], 'YLim', [-1.15 1.15]);
end
%=============================================================================
function hands = worldClockCreateHands(axesHandle)
  hold(axesHandle, 'on');
  hands.hour = plot(axesHandle, [0 0], [0 0.5], 'Color', [0.95 0.96 1.00], ...
    'LineWidth', 4.0);
  hands.minute = plot(axesHandle, [0 0], [0 0.75], ...
    'Color', [0.70 0.85 1.00], 'LineWidth', 2.5);
  hands.second = plot(axesHandle, [0 0], [0 0.85], ...
    'Color', [0.95 0.45 0.35], 'LineWidth', 1.3);
  plot(axesHandle, 0, 0, 'o', 'MarkerFaceColor', [0.95 0.96 1.00], ...
    'MarkerEdgeColor', 'none', 'MarkerSize', 7);
  hold(axesHandle, 'off');
end
%=============================================================================
function worldClockUpdateHands(hands, timeVector)
  hourValue = mod(timeVector(4), 12);
  minuteValue = timeVector(5);
  secondValue = timeVector(6);
  hourAngle = pi / 2 - (hourValue + minuteValue / 60) * pi / 6;
  minuteAngle = pi / 2 - (minuteValue + secondValue / 60) * pi / 30;
  secondAngle = pi / 2 - secondValue * pi / 30;
  set(hands.hour, 'XData', [0 0.5 * cos(hourAngle)], ...
    'YData', [0 0.5 * sin(hourAngle)]);
  set(hands.minute, 'XData', [0 0.75 * cos(minuteAngle)], ...
    'YData', [0 0.75 * sin(minuteAngle)]);
  set(hands.second, 'XData', [0 0.85 * cos(secondAngle)], ...
    'YData', [0 0.85 * sin(secondAngle)]);
end
%=============================================================================
